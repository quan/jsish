(*
Copyright (c) 2017 Minh-Quan Tran
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

use "ast.sml";

exception LeftHandSide of string
exception Expect of (string * string)

val ID_OPTIONAL = false
val ID_REQUIRED = true

val multOps = [TK_TIMES, TK_DIVIDE, TK_MOD]
val addOps = [TK_PLUS, TK_MINUS]
val relationalOps = [TK_LT, TK_GT, TK_LE, TK_GE]
val equalityOps = [TK_EQ, TK_NE]

(* Reverses a list that is second in a tuple. *)
fun returnRev (token, ls) = (token, rev ls)

(* Checks if the given token is a valid starting token for an assignment expression. *)
fun beginsAssignmentExpr (TK_ID _) = true
  | beginsAssignmentExpr (TK_NUM _) = true
  | beginsAssignmentExpr (TK_STRING _) = true
  | beginsAssignmentExpr TK_TRUE = true
  | beginsAssignmentExpr TK_FALSE = true
  | beginsAssignmentExpr TK_UNDEFINED = true
  | beginsAssignmentExpr TK_FUNCTION = true
  | beginsAssignmentExpr TK_LPAREN = true
  | beginsAssignmentExpr TK_THIS = true
  | beginsAssignmentExpr TK_LBRACE = true
  | beginsAssignmentExpr TK_NEW = true
  | beginsAssignmentExpr other = isUnaryOp other

(* Checks if the given token is a valid starting token for a statement. *)
fun beginsStatement TK_IF = true
  | beginsStatement TK_PRINT = true
  | beginsStatement TK_WHILE = true
  | beginsStatement TK_RETURN = true
  | beginsStatement TK_LBRACE = true
  | beginsStatement TK_FUNCTION = false
  | beginsStatement other = beginsAssignmentExpr other orelse isUnaryOp other

(* Checks if the given token is a valid starting token for a source element. *)
fun beginsSourceElem TK_VAR = true
  | beginsSourceElem TK_FUNCTION = true
  | beginsSourceElem other = beginsStatement other

(* Reads the next token from the file stream if the current token matches what
   is expected according to the jsish grammar. *)
fun expect expected token fstream = (
    (*printLn ("expecting " ^ tokenToString expected);*)
    if token = expected
    then nextToken fstream
    else raise Expect (tokenToString expected, tokenToString token)
)

(* Parses a binary operator and reads the next token from the file stream. *)
fun expectOp operators token fstream =
    if List.exists (fn opr => token = opr) operators
    then (nextToken fstream, tokenToBinaryOp token)
    else raise Expect ("operator", tokenToString token)

(* Parses an identifier and reads the next token from the file stream. *)
fun expectId (TK_ID id) fstream = (nextToken fstream, EXP_ID id)
  | expectId other fstream = raise Expect ("identifier", tokenToString other)

(* Parses a object property name and reads the next token from the file stream. *)
fun expectProp (TK_ID id) fstream = (nextToken fstream, EXP_PROP id)
  | expectProp other fstream = raise Expect ("identifier", tokenToString other)

(* Parses an optional identifier. *)
fun optionalId (TK_ID id) fstream = (nextToken fstream, EXP_ID id)
  | optionalId other fstream = (other, EXP_UNDEFINED)

(*  Parses a boolean value from the file stream. *)
fun parseBool true fstream = (nextToken fstream, EXP_TRUE)
  | parseBool false fstream = (nextToken fstream, EXP_FALSE)

(*  Parses an expression consisting of comma-separated expressions to be parsed
    by f, returnin a list containing elements that pass the given predicate. *)
fun parseCommaSeparatedExpr f pred token fstream ls =
    let
        val (tkn, expr) = f token fstream
        val newLs = if pred expr
            then expr::ls
            else ls
    in
        if tkn = TK_COMMA
        then parseCommaSeparatedExpr f pred (nextToken fstream) fstream newLs
        else (tkn, newLs)
    end

(* Parses a expression of a type with the given [operators] and at least one
   sub expression to be parsed by the given function [subExpr], building an
   abstract syntax tree. *)
fun parseRepeatExpr operators subExpr token fstream lft =
    if List.exists (fn opr => token = opr) operators
    then
        let
            val (tkn0, opr) = expectOp operators token fstream
            val (tkn1, rht) = subExpr tkn0 fstream
            val recurse = parseRepeatExpr operators subExpr
        in
            recurse tkn1 fstream (EXP_BINARY {opr=opr, lft=lft, rht=rht})
        end
    else (token, lft)

(* Parses a binary expression defined by the given [operators] and with sub
   expressions parsed by the given function [subExpr]. *)
fun parseBinaryExpr operators subExpr token fstream =
    let
        val (tkn, lft) = subExpr token fstream
    in
        parseRepeatExpr operators subExpr tkn fstream lft
    end

(*  Parses any number of object members and returns them as a list. *)
fun parseMembers token fstream obj =
    if token = TK_DOT
    then
        let
            val (tkn, prop) = expectProp (nextToken fstream) fstream
        in
            parseMembers tkn fstream (EXP_MEMBER {obj=obj, prop=prop})
        end
    else (token, obj)

(*  Parses a parenthesized assignment expression. *)
fun parseParenExpr fstream =
    let
        val (tkn, expr) = parseExpr (nextToken fstream) fstream
    in
        (expect TK_RPAREN tkn fstream, expr)
    end

(*  Parses a function expression, which consists of
    - the function keyword
    - an optional identifier
    - an open parenthesis
    - a list of parameters
    - a close parenthesis
    - an open brace
    - some number of source elements
    - a close brace.
    *)
and parseFunction idReq fstream =
    let
        (* Read the function keyword. *)
        val tkn0 = nextToken fstream
        val (tkn1, id) = (if idReq then expectId else optionalId) tkn0 fstream
        val tkn2 = expect TK_LPAREN tkn1 fstream
        val (tkn3, params) = parseParams tkn2 fstream
        val tkn4 = expect TK_RPAREN tkn3 fstream
        val tkn5 = expect TK_LBRACE tkn4 fstream
        val (tkn6, body) = parseSourceElems tkn5 fstream
        val tkn7 = expect TK_RBRACE tkn6 fstream
    in
        (tkn7, EXP_FUNCTION {id=id, params=params, body=body})
    end

(*  Parses a property assignment, which consists of
    - an identifier
    - an assignment expression bound to that identifier
    *)
and parsePropertyAssignment token fstream = 
    let
        fun extractId (EXP_ID st) = st
        val (tkn0, id) = expectId token fstream
        val tkn1 = expect TK_COLON tkn0 fstream
        val (tkn2, expr) = parseAssignmentExpr tkn1 fstream
    in
        (tkn2, (extractId id, expr))
    end

(*  Parses a property list, which consists of at least one property assignment. *)
and parsePropertyList token fstream = returnRev (
    parseCommaSeparatedExpr parsePropertyAssignment (fn _ => true) token fstream []
)

(*  Parses an object literal, which consists of
    - a left curly boi
    - an optional list of property names and values
    - a right curly boi
    *)
and parseObjLiteral fstream =
    let
        fun beginsPropertyAssignment (TK_ID _) = true
          | beginsPropertyAssignment _ = false

        (* Read the opening curly brace. *)
        val tkn0 = nextToken fstream
        val (tkn1, props) = if beginsPropertyAssignment tkn0
            then parsePropertyList tkn0 fstream
            else (tkn0, [])
    in
        (expect TK_RBRACE tkn1 fstream, EXP_OBJ_LIT {props=props})
    end

(*  Parses a primary expression, which is one of
    - an expression in (parentheses)
    - an identifier
    - a <number>
    - a boolean <true> or <false>
    - a <string>
    - a function (expression)
    - <undefined>
    - this
    - an object literal
    *)
and parsePrimaryExpr TK_LPAREN fstream = parseParenExpr fstream
  | parsePrimaryExpr (TK_ID id) fstream = expectId (TK_ID id) fstream
  | parsePrimaryExpr (TK_NUM num) fstream = (nextToken fstream, EXP_NUM num)
  | parsePrimaryExpr (TK_STRING st) fstream = (nextToken fstream, EXP_STRING st)
  | parsePrimaryExpr TK_TRUE fstream = (nextToken fstream, EXP_TRUE)
  | parsePrimaryExpr TK_FALSE fstream = (nextToken fstream, EXP_FALSE)
  | parsePrimaryExpr TK_FUNCTION fstream = parseFunction ID_OPTIONAL fstream
  | parsePrimaryExpr TK_UNDEFINED fstream = (nextToken fstream, EXP_UNDEFINED)
  | parsePrimaryExpr TK_THIS fstream = (nextToken fstream, EXP_THIS)
  | parsePrimaryExpr TK_LBRACE fstream = parseObjLiteral fstream
  | parsePrimaryExpr token fstream = raise Expect ("value", tokenToString token)

(*  Parses the construction of an object. *)
and parseObjConstruction token fstream =
    let
        val tkn0 = expect TK_NEW token fstream
        val (tkn1, constr) = parseMemberExpr tkn0 fstream
        val (tkn2, args) = parseArgs tkn1 fstream
    in
        (tkn2, EXP_OBJ_CONSTR {constr=constr, args=args})
    end

(*  Parses a member expression, which consists of
    - either
        - a primary expression
        or
        - the new keyword
        - a member expression
        - arguments
    - any number of optional .id members 
    *)
and parseMemberExpr token fstream =
    let
        val (tkn0, expr) = if token = TK_NEW 
            then parseObjConstruction token fstream
            else parsePrimaryExpr token fstream
    in
        if tkn0 = TK_DOT
        then parseMembers tkn0 fstream expr
        else (tkn0, expr)
    end

(*  Parses arguments, building on the list provided. Consists of
    - any number of assignment expressions
    *)
and parseArgList token fstream =
    let
        val parseArgListRev = parseCommaSeparatedExpr parseAssignmentExpr (fn _ => true)
    in
        returnRev (parseArgListRev token fstream [])
    end

(*  Parses arguments provided to a function call, returning a list of expressions. *)
and parseArgs token fstream = 
    let
        val tkn0 = expect TK_LPAREN token fstream
        val (tkn1, args) = if beginsAssignmentExpr tkn0
            then parseArgList tkn0 fstream
            else (tkn0, [])
    in
        (expect TK_RPAREN tkn1 fstream, args)
    end

(*  Parses member access of an object. *)
and parseMemberAccess obj token fstream =
    let
        val tkn0 = expect TK_DOT token fstream
        val (tkn1, prop) = expectProp tkn0 fstream
    in
        parseCallOrProp (EXP_MEMBER {obj=obj, prop=prop}) tkn1 fstream
    end

(*  Parses a function call. *)
and parseFunctionCall func token fstream = 
    let
        val (tkn0, args) = parseArgs token fstream
    in
        parseCallOrProp (EXP_CALL {func=func, args=args}) tkn0 fstream
    end

(*  Parses a function call or a member access. *)
and parseCallOrProp expr token fstream = 
    case token of
        TK_LPAREN => parseFunctionCall expr token fstream
      | TK_DOT => parseMemberAccess expr token fstream
      | _ => (token, expr)

(*  Parses a call expression, which consists of
    - a member expression
    - optional arguments (for a function call)
    *)
and parseCallExpr token fstream = 
    let
        val (tkn0, expr) = parseMemberExpr token fstream
    in
        parseCallOrProp expr tkn0 fstream
    end

(*  Parses a left-hand side expression, which consists of
    - a call expression
    *)
and parseLHSExpr token fstream = parseCallExpr token fstream

(*  Parses an expression with a unary operation. *)
and parseUnaryOp token fstream =
    let
        val opr = tokenToUnaryOp token
        val (tkn, expr) = parseLHSExpr (nextToken fstream) fstream
    in 
        (tkn, EXP_UNARY {opr=opr, opnd=expr})
    end

(*  Parses a unary expression, which consists of
    - an optional <unary operator>
    - a left-hand side expression
    *)
and parseUnaryExpr token fstream =
    if isUnaryOp token
    then parseUnaryOp token fstream
    else parseLHSExpr token fstream

(*  Parses a multiplicative expression, which consists of
    - a unary expression
    - any number of additional <multiply op>-separated unary expressions
    *)
and parseMultExpr token fstream = parseBinaryExpr multOps parseUnaryExpr token fstream

(*  Parses an additive expression, which consists of
    - a multiplicative expression
    - any number of additional <add op>-separated multiplicative expressions
    *)
and parseAddExpr token fstream = parseBinaryExpr addOps parseMultExpr token fstream

(*  Parses a relational expression, which consists of
    - an additive expression
    - any number of additional <relational op>-separated additive expressions
    *)
and parseRelationalExpr token fstream = parseBinaryExpr relationalOps parseAddExpr token fstream

(* Parses an equality expression, which consists of
    - a relational expression
    - any number of additional <equal op>-separated relational expressions
    *)
and parseEqExpr token fstream = parseBinaryExpr equalityOps parseRelationalExpr token fstream

(*  Parses a logical AND expression, which consists of
    - an equality expression
    - any number of additional &&-separated equality expressions
    *)
and parseAndExpr token fstream = parseBinaryExpr [TK_AND] parseEqExpr token fstream

(*  Parses a logical OR expression, which consists of
    - a logical AND expression
    - any number of additional ||-separated logical AND expressions
    *)
and parseOrExpr token fstream = parseBinaryExpr [TK_OR] parseAndExpr token fstream

(*  Parses a ternary expression, which consists of
    - an assignment expression
    - a colon
    - an assignment expression
    *)
and parseTernaryExpr token fstream guard = 
    let
        val tkn0 = expect TK_QUESTION token fstream
        val (tkn1, thenExpr) = parseAssignmentExpr tkn0 fstream
        val tkn2 = expect TK_COLON tkn1 fstream
        val (tkn3, elseExpr) = parseAssignmentExpr tkn2 fstream
    in
        (tkn3, EXP_COND {guard=guard, thenExpr=thenExpr, elseExpr=elseExpr})
    end

(*  Parses a conditional expression, which consists of
    - a logical OR expression
    - an optional ternary expression
    *)
and parseConditionalExpr token fstream  =
    let
        val (tkn0, expr) = parseOrExpr token fstream
    in  (* Parse an optional ternary operation. *)
        if tkn0 = TK_QUESTION
        then parseTernaryExpr tkn0 fstream expr
        else (tkn0, expr)
    end

(*  Parses an assignment operation in an assignment expression, assigning an
    expression to a given LHS expression. *)
and parseAssignmentOp token fstream lhs = 
    let (* Only identifiers and object members are valid lhs expressions. *)
        val lhsExpr = case lhs of
            EXP_ID _ => lhs
          | EXP_MEMBER _ => lhs
          | _ => raise LeftHandSide "="
        val tkn0 = expect TK_ASSIGN token fstream
        val (tkn1, rhsExpr) = parseAssignmentExpr tkn0 fstream
    in
        (tkn1, EXP_ASSIGN {lhs=lhsExpr, rhs=rhsExpr})
    end

(*  Parses an assignment expression, which consists of
    - a conditional expression
    - an optional assignment operator and assignment expressions
    *)
and parseAssignmentExpr token fstream =
    let
        val (tkn0, expr) = parseConditionalExpr token fstream
    in
        if tkn0 = TK_ASSIGN
        then parseAssignmentOp tkn0 fstream expr
        else (tkn0, expr)
    end

(*  Parses an expression, which consists of
    - one assignment expression
    - any number of additional comma-separated assignment expressions
    *)
and parseExpr token fstream = parseBinaryExpr [TK_COMMA] parseAssignmentExpr token fstream

(*  Parses an expression statement, which consists of
    - an expression
    - a semicolon
    *)
and parseExprStatement token fstream =
    let
        val (tkn0, expr) = parseExpr token fstream
        val tkn1 = expect TK_SEMI tkn0 fstream
    in 
        (tkn1, ST_EXP {expr=expr})
    end

(*  Parses some number (0+) of statements contained in a block statement and
    returns them as a list. *)
and parseStatementBlock token fstream =
    let
        fun parseStatementBlockRev token fstream stmts =
            if beginsStatement token
            then 
                let
                    val (tkn, stmt) = parseStatement token fstream
                    val newStmts = stmt::stmts
                in
                    parseStatementBlockRev tkn fstream newStmts
                end
            else (token, stmts)
    in
        returnRev (parseStatementBlockRev token fstream [])
    end

(*  Parses a block statement, which consists of
    - a left curly brace
    - any number of statements
    - a right curly brace
    *)
and parseBlockStatement token fstream = 
    let
        val tkn0 = expect TK_LBRACE token fstream
        val (tkn1, stmts) = parseStatementBlock tkn0 fstream
        val tkn2 = expect TK_RBRACE tkn1 fstream
    in
        (tkn2, BLOCK_ST {stmts=stmts})
    end

(*  Parses an if statement, which consists of
    - an if token
    - a parenthesized guard (expression)
    - a then {block statement} in curly braces
    - an optional else token and {block statement} in curly braces
    *)
and parseIfStatement token fstream =
    let
        val tkn0 = expect TK_IF token fstream
        val tkn1 = expect TK_LPAREN tkn0 fstream
        val (tkn2, guard) = parseExpr tkn1 fstream
        val tkn3 = expect TK_RPAREN tkn2 fstream
        val (tkn4, thenBlock) = parseBlockStatement tkn3 fstream
    in  
        (* Parse an optional else clause. *)
        if tkn4 = TK_ELSE
        then
            let 
                val tkn5 = expect TK_ELSE tkn4 fstream
                val (tkn6, elseBlock) = parseBlockStatement tkn5 fstream
            in
                (tkn6, IF_ST {guard=guard, thenBlock=thenBlock, elseBlock=elseBlock})
            end
        else (tkn4, IF_ST {guard=guard, thenBlock=thenBlock, elseBlock=BLOCK_ST {stmts=[]}})
    end

(*  Parses a print statement, which consists of
    - a print token
    - an expression
    - a semicolon
    *)
and parsePrintStatement token fstream = 
    let
        val tkn0 = expect TK_PRINT token fstream
        val (tkn1, expr) = parseExpr tkn0 fstream
        val tkn2 = expect TK_SEMI tkn1 fstream
    in
        (tkn2, PRINT_ST {expr=expr})
    end

(*  Parses a while statement, which consists of
    - a while token
    - a parenthesized conditional (expression)
    - a {block statement}
    *)
and parseWhileStatement token fstream =
    let
        val tkn0 = expect TK_WHILE token fstream
        val tkn1 = expect TK_LPAREN tkn0 fstream
        val (tkn2, guard) = parseExpr tkn1 fstream
        val tkn3 = expect TK_RPAREN tkn2 fstream
        val (tkn4, block) = parseBlockStatement tkn3 fstream
    in
        (tkn4, WHILE_ST {guard=guard, block=block})
    end

(*  Parses a return statement, which consists of
    - the return keyword
    - an optional expression
    - a semicolon 
    *)
and parseReturnStatement token fstream =
    let
        val tkn0 = expect TK_RETURN token fstream
        val (tkn1, expr) = if beginsAssignmentExpr tkn0
            then parseExpr tkn0 fstream
            else (tkn0, EXP_UNDEFINED)
    in
        (expect TK_SEMI tkn1 fstream, RETURN_ST {expr=expr})
    end

(*  Parses a statement, which consists of either
    - a block statement
    - an if statement
    - a print statement
    - an iteration (while) statement
    - a return statement
    - a garbage collection statement
    - an inUse statement
    - an expression statement
    *)
and parseStatement token fstream =
    (* This is a case because we need access to the token in the next step. *)
    case token of
        TK_LBRACE => parseBlockStatement token fstream
      | TK_IF => parseIfStatement token fstream
      | TK_PRINT => parsePrintStatement token fstream
      | TK_WHILE => parseWhileStatement token fstream
      | TK_RETURN => parseReturnStatement token fstream
      | _ => parseExprStatement token fstream

(*  Parses a variable initialization for the given id. *)
and parseVarInitialiation id token fstream =
    let
        val tkn0 = expect TK_ASSIGN token fstream
        val (tkn1, expr) = parseAssignmentExpr tkn0 fstream
    in
        (tkn1, VAR_INIT {id=id, expr=expr})
    end

(*  Parses a variable declaration or initialization, which consists of
    - an identifier
    - an optional assignment
    *)
and parseVarDeclaration token fstream =
    let
        val (tkn0, id) = expectId token fstream
    in
        if tkn0 = TK_ASSIGN
        then parseVarInitialiation id tkn0 fstream
        else (tkn0, VAR_DEC {id=id})
    end

(*  Parses a variable declaration list, which consists of
    - a variable declaration
    - any number of additional comma-separated variable declarations
    *)
and parseVarList token fstream vars = 
    parseCommaSeparatedExpr parseVarDeclaration (fn _ => true) token fstream []

(*  Parses a variable element, which consists of
    - the var keyword
    - a variable declaration list
    - a semicolon
    *)
and parseVarElement token fstream =
    let
        (* Build a list from parsing some number of variable declarations. *)
        val tkn0 = expect TK_VAR token fstream
        val (tkn1, vars) = returnRev (parseVarList tkn0 fstream [])
        val tkn2 = expect TK_SEMI tkn1 fstream
    in
        (tkn2, VAR_ELEM {vars=vars})
    end

(*  Parses a parameter list, building on the list provided. Consists of
    - any number of identifiers
    *)
and parseParams token fstream =
    let
        val parseParamsRev = parseCommaSeparatedExpr optionalId (fn id => id <> EXP_UNDEFINED)
    in
        returnRev (parseParamsRev token fstream [])
    end

(*  Parses a function declaration, which is a wrapper for a function expression
    with a required identifier.
    *)
and parseFunDeclaration token fstream = 
    let
        val (tkn, func) = parseFunction ID_REQUIRED fstream
    in
        (tkn, FN_DEC func)
    end

(*  Parses a source element, which consists of either
    - a function declaration
    - a variable element
    or
    - a statement
    *)
and parseSourceElement token fstream = (
    case token of
        TK_FUNCTION => parseFunDeclaration token fstream
      | TK_VAR => parseVarElement token fstream
      | _ => 
        let
            val (tkn, stmt) = parseStatement token fstream
        in
            (tkn, STMT {stmt = stmt})
        end)

(*  Parses any number of source elements. *)
and parseSourceElems token fstream =
    let
        (* Parse source elements, building a list in reverse. *)
        fun parseSourceElemsRev token fstream elems =
            if beginsSourceElem token
            then 
                let 
                    val (tkn0, elem) = parseSourceElement token fstream
                    val newElems = elem::elems
                in
                    parseSourceElemsRev tkn0 fstream newElems
                end
            else (token, elems)
    in 
        returnRev (parseSourceElemsRev token fstream [])
    end

(* Parses a program, building upon the provided list of source elements. *)
and parseProgramAST token fstream =
    let
        val (tkn0, elems) = parseSourceElems token fstream
    in
        (tkn0, PROGRAM {elems=elems})
    end

(*  Parses a program, which consists of
    - any number of source elements
    *)
fun parseProgram token fstream = parseProgramAST token fstream

(*  Tokenize and parse a jsish program contained in the given filename. *)
fun parse filename = 
    let
        val fstream = TextIO.openIn filename
        val first = nextToken fstream
        val (final, ast) = parseProgram first fstream
        val eof = expect TK_EOF final fstream
    in 
        ast
    end
    handle LeftHandSide token => error ("unexpected token " ^ sq token)
         | Expect (expected, found) => error ("expected " ^ sq expected ^ ", found " ^ sq found)
         | Conversion token => error ("invalid operator " ^ sq token)

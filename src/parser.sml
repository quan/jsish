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

use "tokenizer.sml";
use "ast.sml";

exception LeftHandSide of string
exception Expect of (string * string)

val ID_OPTIONAL = false
val ID_REQUIRED = true

(* Reverses a list in a tuple. *)
fun returnRev (token, ls) = (token, rev ls)

(* Checks if the given token is a valid starting token for an assignment expression. *)
fun beginsAssignmentExpr (TK_ID _) = true
  | beginsAssignmentExpr (TK_NUM _) = true
  | beginsAssignmentExpr (TK_BOOL _) = true
  | beginsAssignmentExpr (TK_STRING _) = true
  | beginsAssignmentExpr TK_UNDEFINED = true
  | beginsAssignmentExpr TK_FUNCTION = true
  | beginsAssignmentExpr TK_LPAREN = true
  | beginsAssignmentExpr _ = false

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

(* Prints an error when the token found does not match what was expected. *)
fun expectErrorStr expectedStr found =
    let
        val foundStr = tokenToString found
    in
        error ("expected '" ^ expectedStr ^ "', found '" ^ foundStr ^ "'") 
    end

(* Prints an error when the token found does not match what was expected. *)
fun expectError expected found = expectErrorStr (tokenToString expected) found

(* Reads the next token from the file stream if the current token matches what
   is expected according to the jsish grammar. *)
fun expect expected token fstream = (
    if token = expected
    then nextToken fstream
    else expectError expected token
)

(* Parses an operator and reads the next token from the file stream. *)
fun expectOp operators token fstream =
    if List.exists (fn opr => token = opr) operators
    then (nextToken fstream, tknToBop token)
    else expectErrorStr "operator" token

(* Parses an identier and reads the next token from the file stream. *)
fun expectId (TK_ID id) fstream = (nextToken fstream, EXP_ID id)
  | expectId other fstream = raise Expect ("identifier", tokenToString other)

(* Parses an optional identifier. *)
fun optionalId (TK_ID id) fstream = (nextToken fstream, EXP_ID id)
  | optionalId other fstream = (other, EXP_UNDEFINED)

(*  Parses a boolean value from the file stream. *)
fun parseBool true fstream = (nextToken fstream, EXP_TRUE)
  | parseBool false fstream = (nextToken fstream, EXP_FALSE)

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

(*  Parses a parenthesized assignment expression. *)
fun parseParenExpr fstream =
    let
        (* Read the left parenthesis. *)
        val tkn0 = nextToken fstream
        val (tkn1, expr) = parseExpr tkn0 fstream
    in
        (expect TK_RPAREN tkn1 fstream, expr)
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
        (tkn7, EXP_CLOSURE {id=id, params=params, body=body})
    end

(*  Parses a primary expression, which is one of
    - an expression in (parentheses)
    - an identifier
    - a <number>
    - a boolean <true> or <false>
    - a <string>
    - a function (expression)
    - <undefined>
    *)
and parsePrimaryExpr TK_LPAREN fstream = parseParenExpr fstream
  | parsePrimaryExpr (TK_ID id) fstream = expectId (TK_ID id) fstream
  | parsePrimaryExpr (TK_NUM num) fstream = (nextToken fstream, EXP_NUM num)
  | parsePrimaryExpr (TK_BOOL bln) fstream = parseBool bln fstream
  | parsePrimaryExpr (TK_STRING st) fstream = (nextToken fstream, EXP_STRING st)
  | parsePrimaryExpr TK_FUNCTION fstream = parseFunction ID_OPTIONAL fstream
  | parsePrimaryExpr TK_UNDEFINED fstream = (nextToken fstream, EXP_UNDEFINED)
  | parsePrimaryExpr token fstream = expectErrorStr "value" token

(*  Parses a member expression, which consists of 
    - a primary expression
    *)
and parseMemberExpr token fstream = parsePrimaryExpr token fstream

(*  Parses arguments, building on the list provided. Consists of
    - any number of assignment expressions
    *)
and parseArgList token fstream =
    let
        fun parseArgListRev token fstream ls =
            let (* Parse an assignment expression and add it to the front of the list. *)
                val (tkn, expr) = parseAssignmentExpr token fstream
                val newLs = expr::ls
            in  (* Parse an additional assignment expression or return the list. *)
                if tkn = TK_COMMA
                then parseArgListRev (expect TK_COMMA tkn fstream) fstream newLs
                else (tkn, newLs)
            end
    in  (* Reverse and return the list. *)
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

(*  Parses a function call... *)
and parseFunctionCall expr token fstream =
    if token = TK_LPAREN
    then
        let
            val (tkn0, args) = parseArgs token fstream
            val (tkn1, nextExpr) = parseFunctionCall (EXP_FN_CALL {id=expr, args=args}) tkn0 fstream
        in
            (tkn1, nextExpr)
        end
    else (token, expr)


(*  Parses a call expression, which consists of
    - a member expression
    - optional arguments (for a function call)
    *)
and parseCallExpr token fstream = 
    let
        val (tkn0, expr) = parseMemberExpr token fstream
    in
        parseFunctionCall expr tkn0 fstream
    end

(*  Parses a left-hand side expression, which consists of
    - a call expression
    *)
and parseLHSExpr token fstream = parseCallExpr token fstream

(*  Parses a unary expression, which consists of
    - an optional <unary operator>
    - a left-hand side expression
    *)
and parseUnaryExpr token fstream =
    if isUnaryOp token
    then
        let
            (* Read the operator. *)
            val opr = tknToUop token
            val tkn0 = nextToken fstream
            val (tkn1, expr) = parseLHSExpr tkn0 fstream
        in 
            (tkn1, EXP_UNARY {opr=opr, opnd=expr})
        end
    else parseLHSExpr token fstream

(*  Parses a multiplicative expression, which consists of
    - a unary expression
    - any number of additional <multiply op>-separated unary expressions
    *)
and parseMultExpr token fstream =
    parseBinaryExpr [TK_TIMES, TK_DIVIDE, TK_MOD] parseUnaryExpr token fstream

(*  Parses an additive expression, which consists of
    - a multiplicative expression
    - any number of additional <add op>-separated multiplicative expressions
    *)
and parseAddExpr token fstream =
    parseBinaryExpr [TK_PLUS, TK_MINUS] parseMultExpr token fstream

(*  Parses a relational expression, which consists of
    - an additive expression
    - any number of additional <relational op>-separated additive expressions
    *)
and parseRelationalExpr token fstream =
    parseBinaryExpr [TK_LT, TK_GT, TK_LE, TK_GE] parseAddExpr token fstream

(* Parses an equality expression, which consists of
    - a relational expression
    - any number of additional <equal op>-separated relational expressions
    *)
and parseEqExpr token fstream =
    parseBinaryExpr [TK_EQ, TK_NE] parseRelationalExpr token fstream

(*  Parses a logical AND expression, which consists of
    - an equality expression
    - any number of additional &&-separated equality expressions
    *)
and parseAndExpr token fstream =
    parseBinaryExpr [TK_AND] parseEqExpr token fstream

(*  Parses a logical OR expression, which consists of
    - a logical AND expression
    - any number of additional ||-separated logical AND expressions
    *)
and parseOrExpr token fstream =
    parseBinaryExpr [TK_OR] parseAndExpr token fstream

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

(*  Parses an assignment operation in an assignment expression, assigning a
    parsed expression to a given LHS expression. *)
and parseAssignmentOp token fstream (EXP_ID id) =
    let
        val lhsExpr = EXP_ID id
        val tkn0 = expect TK_ASSIGN token fstream
        val (tkn1, rhsExpr) = parseAssignmentExpr tkn0 fstream
    in
        (tkn1, EXP_ASSIGN {lhs=lhsExpr, rhs=rhsExpr})
    end
  | parseAssignmentOp token fstream other = raise LeftHandSide ("unexpected token '='")

(*  Parses an assignment expression, which consists of
    - a conditional expression
    - an optional assignment operator and assignment expressions
    *)
and parseAssignmentExpr token fstream =
    let
        val (tkn0, expr) = parseConditionalExpr token fstream
    in  (* Parse an optional assignment. *)
        if tkn0 = TK_ASSIGN
        then parseAssignmentOp tkn0 fstream expr
        else (tkn0, expr)
    end

(*  Parses an expression, which consists of
    - one assignment expression
    - any number of additional comma-separated assignment expressions
    *)
and parseExpr token fstream =
    parseBinaryExpr [TK_COMMA] parseAssignmentExpr token fstream

(*  Parses an expression statement, which consists of
    - an expression
    - a semicolon
    *)
and parseExprStatement token fstream =
    let
        val (tkn0, expr) = parseExpr token fstream
        val tkn1 = expect TK_SEMI tkn0 fstream
    in 
        (tkn1, expr)
    end

(*  Parses some number (0+) of statements contained in a block statement and
    returns them as a list. *)
and parseBlockStatements token fstream =
    let
        fun parseBlockStatementsRev token fstream stmts =
            if beginsStatement token
            then (* Parse a statement if one is contained in the block. *)
                let
                    val (tkn, stmt) = parseStatement token fstream
                    val newStmts = stmt::stmts (* NOTE: Reverse order *)
                in
                    parseBlockStatementsRev tkn fstream newStmts
                end
            else (token, stmts)
    in
        returnRev (parseBlockStatementsRev token fstream [])
    end

(*  Parses a block statement, which consists of
    - a left curly brace
    - any number of statements
    - a right curly brace
    *)
and parseBlockStatement token fstream = 
    let
        val tkn0 = expect TK_LBRACE token fstream
        val (tkn1, stmts) = parseBlockStatements tkn0 fstream
        val block = BLOCK_ST {stmts=stmts}
        val tkn2 = expect TK_RBRACE tkn1 fstream
    in
        (tkn2, block)
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
    in  (* Parse an optional else clause. *)
        if tkn4 = TK_ELSE
        then
            let 
                val tkn5 = expect TK_ELSE tkn4 fstream
                val (tkn6, elseBlock) = parseBlockStatement tkn5 fstream
            in
                (tkn6, IF_ST {guard=guard, thenBlock=thenBlock, elseBlock=elseBlock})
            end
        else 
            let
                val noElse = BLOCK_ST {stmts=[]}
            in
                (tkn4, IF_ST {guard=guard, thenBlock=thenBlock, elseBlock=noElse})
            end
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
    - a semicolon*)
and parseReturnStatement token fstream =
    let
        val tkn0 = expect TK_RETURN token fstream
        val (tkn1, value) = if beginsAssignmentExpr tkn0
            then parseExpr tkn0 fstream
            else (tkn0, EXP_UNDEFINED)
    in
        (expect TK_SEMI tkn1 fstream, RETURN_ST {value=value})
    end

(*  Parses a statement, which consists of either
    - a block statement
    - an if statement
    - a print statement
    - an iteration statement
    - an expression statement
    *)
and parseStatement token fstream =
    case token of
        TK_LBRACE => parseBlockStatement token fstream
      | TK_IF => parseIfStatement token fstream
      | TK_PRINT => parsePrintStatement token fstream
      | TK_WHILE => parseWhileStatement token fstream
      | TK_RETURN => parseReturnStatement token fstream
      | other =>
        let
            val (tkn, expr) = parseExprStatement token fstream
        in
            (tkn, ST_EXP {expr=expr})
        end

(*  Parses a variable declaration, which consists of
    - an identifier
    - an optional assignment
    *)
and parseVarDeclaration token fstream =
    let
        val (tkn0, id) = expectId token fstream
        val (tkn1, value) = (
            if tkn0 = TK_ASSIGN
            then parseAssignmentOp tkn0 fstream id
            else (tkn0, EXP_UNDEFINED)
        )
    in
        (tkn1, VAR_DEC {id=id, value=value})
    end

(*  Parses a variable declaration list, which consists of
    - a variable declaration
    - any number of additional comma-separated variable declarations
    *)
and parseVarList token fstream vars =
    let (* Parse a variable declaration and add it to the list. *)
        val (tkn, var) = parseVarDeclaration token fstream
        val newVars = var::vars
    in  (* Parse an additional variable declaration or return the list. *)
        if tkn = TK_COMMA
        then parseVarList (expect TK_COMMA tkn fstream) fstream newVars
        else (tkn, newVars)
    end

(*  Parses a variable element, which consists of
    - the var keyword
    - a variable declaration list
    - a semicolon
    *)
and parseVarElement token fstream =
    let (* Build a list from parsing some number of variable declarations. *)
        val tkn0 = expect TK_VAR token fstream
        val (tkn1, vars) = parseVarList tkn0 fstream []
        val tkn2 = expect TK_SEMI tkn1 fstream
    in
        (tkn2, VAR_ELEM {vars=(rev vars)})
    end

(*  Parses a parameter list, building on the list provided. Consists of
    - any number of identifiers
    *)
and parseParams token fstream =
    let
        fun parseParamsRev token fstream ids = 
            let
                val (tkn, id) = optionalId token fstream
                val newIds = if id = EXP_UNDEFINED
                    then ids
                    else id::ids
            in
                if tkn = TK_COMMA
                then parseParamsRev (expect TK_COMMA tkn fstream) fstream newIds
                else (tkn, newIds)
            end
    in
        returnRev (parseParamsRev token fstream [])
    end

(*  Parses a function declaration, which consists of
    - the function keyword
    - an identifier
    - a left parenthesis
    - a parameter list
    - a right parenthesis
    - a left brace *)
and parseFunDeclaration token fstream = 
    let
        val (tkn, func) = parseFunction ID_REQUIRED fstream
    in
        (tkn, FN_DEC func)
    end

(*  Parses any number of source elements, building upon the list provided. *)
and parseSourceElems token fstream =
    let (* Parse source elements, building a list in reverse. *)
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
    in  (* Reverse and return the list. *)
        returnRev (parseSourceElemsRev token fstream [])
    end

(*  Parses a source element, which consists of one of
    - a statement
    - a function declaration
    or
    - a variable element
    *)
and parseSourceElement token fstream = (
    case token of
        TK_FUNCTION => parseFunDeclaration token fstream
      | TK_VAR => parseVarElement token fstream
      | other => 
        let
            val (tkn, stmt) = parseStatement token fstream
        in
            (tkn, STMT {stmt = stmt})
        end
)

(* Parses a program, building upon the provided list of source elements. *)
fun parseProgramAST token fstream =
    let
        fun parseProgramASTrev token fstream elems =
            if beginsSourceElem token
            then
                (* Parse each additional source element, building the element list in reverse. *)
                let
                    val (tkn, elem) = parseSourceElement token fstream
                    val nextElems = elem::elems
                in
                    parseProgramASTrev tkn fstream nextElems
                end
            else (token, PROGRAM {elems=elems})
        fun returnProgramRev (token, PROGRAM {elems=elems}) = (token, PROGRAM {elems=(rev elems)})      
    in
        returnProgramRev (parseProgramASTrev token fstream [])
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
    handle LeftHandSide msg => error msg
         | Expect (expected, found) => error ("expected '" ^ expected ^ "', found '" ^ found ^ "'")

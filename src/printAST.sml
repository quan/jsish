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

use "parser.sml";

val concatWithComma = String.concatWith ", "

val binaryOpStrings = [
  (BOP_PLUS, "+"),
  (BOP_MINUS, "-"),
  (BOP_TIMES, "*"),
  (BOP_DIVIDE, "/"),
  (BOP_MOD, "%"),
  (BOP_EQ, "=="),
  (BOP_NE, "!="),
  (BOP_LT, "<"),
  (BOP_GT, ">"),
  (BOP_LE, "<="),
  (BOP_GE, ">="),
  (BOP_AND, "&&"),
  (BOP_OR, "||"),
  (BOP_COMMA, ",")
]

fun binaryOpToString bop = valOf (findMatch bop binaryOpStrings)

(* Converts a jsish unary operator to a string. *)
fun uopToString UOP_NOT = "!"
  | uopToString UOP_TYPEOF = "typeof"
  | uopToString UOP_MINUS = "-"

(* Converts a jsish binary expression to a string. *)
fun binaryExprString opr lft rht =
    let
        val lStr = exprString lft
        val opStr = binaryOpToString opr
        val rStr = exprString rht
    in
        paren (lStr ^ " " ^ opStr ^ " " ^ rStr)
    end

(* Converts a jsish unary expression to a string. *)
and unaryExprString UOP_TYPEOF opnd = "(typeof " ^ (exprString opnd) ^ ")"
  | unaryExprString opr opnd = paren ((uopToString opr) ^ (exprString opnd))

(* Converts a jsish conditional expression to a string. *)
and conditionalExprString guard thenExpr elseExpr =
    let
        val guardStr = exprString guard
        val thenStr = exprString thenExpr
        val elseStr = exprString elseExpr
    in
        paren (guardStr ^ " ? " ^ thenStr ^ " : " ^ elseStr)
    end

(* Converts a jsish assignment expression to a string. *)
and assignmentExprString lhs rhs = paren ((exprString lhs) ^ " = " ^ (exprString rhs))

(* Converts a jsish parameter list to a string. *)
and paramListString params = (concatWithComma o map exprString) params

(* Converts a jsish function expression to a string. *)
and functionExprString id params elems = 
    let
        val functionHeader = if id = EXP_UNDEFINED
            then "function ("
            else "function " ^ (exprString id) ^ "("
    in 
        (paren o concat) [
            line (functionHeader ^ (paramListString params) ^ ")"),
            line "{",
            concat (map srcElemString elems),
            line "}"
        ]
    end

(* Converts a jsish function call to a string. *)
and functionCallString id args = (exprString id) ^ "(" ^ (paramListString args) ^ ")"

(* Converts a jsishs expression to a string. *)
and exprString (EXP_NUM num) = intToString num
  | exprString (EXP_STRING st) = "\"" ^ (unescape st) ^ "\""
  | exprString (EXP_ID id) = id
  | exprString EXP_TRUE = "true"
  | exprString EXP_FALSE = "false"
  | exprString EXP_UNDEFINED = "undefined"
  | exprString (EXP_BINARY {opr, lft, rht}) = binaryExprString opr lft rht
  | exprString (EXP_UNARY {opr, opnd}) = unaryExprString opr opnd
  | exprString (EXP_COND {guard, thenExpr, elseExpr}) = conditionalExprString guard thenExpr elseExpr
  | exprString (EXP_ASSIGN {lhs, rhs}) = assignmentExprString lhs rhs
  | exprString (EXP_FUNCTION {id, params, body}) = functionExprString id params body
  | exprString (EXP_FN_CALL {func, args}) = functionCallString func args

(* Converts a jsish expression statement to a string. *)
and exprStatementString expr = line ((exprString expr) ^ ";")

(* Converts a jsish print statement to a string. *)
and printStatementString expr = line ("print " ^ (exprString expr) ^ ";")

(* Converts a jsish block statement containing the given statements to a string. *)
and blockStatementString stmts = concat [
    line "{",
    concat (map statementString stmts),
    line "}"
]

(* Converts a jsish while statement to a string. *)
and whileStatementString guard block = concat [
    line ("while (" ^ (exprString guard) ^ ")"),
    statementString block
]

(* Converts a jsish return statement to a string. *)
and returnStatementString value = line ("return " ^ (exprString value) ^ ";")

(* Converts a jsish if statement to a string. *)
and ifStatementString guard thenBlock elseBlock = concat [
    line ("if (" ^ (exprString guard) ^ ")"),
    statementString thenBlock,
    line "else",
    statementString elseBlock
]

(* Converts a jsish statement to a string. *)
and statementString (ST_EXP {expr}) = exprStatementString expr
  | statementString (PRINT_ST {expr}) = printStatementString expr
  | statementString (BLOCK_ST {stmts}) = blockStatementString stmts
  | statementString (WHILE_ST {guard, block}) = whileStatementString guard block
  | statementString (RETURN_ST {expr}) = returnStatementString expr
  | statementString (IF_ST {guard, thenBlock, elseBlock}) =
    ifStatementString guard thenBlock elseBlock

(* Converts a jsish function declaration to a string. *)
and functionDecString (EXP_FUNCTION {id, params, body}) = concat [
    line ("function " ^ (exprString id) ^ "(" ^ (paramListString params) ^ ")"),
    line "{",
    concat (map srcElemString body),
    line "}"
]

(* Converts a jsish variable declaration to a string. *)
and varDecString (VAR_DEC {id, expr=EXP_UNDEFINED}) = exprString id
  | varDecString (VAR_DEC {id, expr=expr}) = (exprString id) ^ " = " ^ (exprString expr)

(* Converts a jsish variable element to a string. *)
and varElemString vars = line ("var " ^ ((concatWithComma o map varDecString) vars ^ ";"))

(* Converts a jsish source element to a string. *)
and srcElemString (STMT {stmt=stmt}) = statementString stmt
  | srcElemString (FN_DEC (expression)) = functionDecString expression
  | srcElemString (VAR_ELEM {vars=vars}) = varElemString vars

(* Returns the string representation of a jsish program. *)
fun programString (PROGRAM {elems}) = concat (map srcElemString elems)

(* Prints the jsish program represented by the given AST. *)
fun printAST program = printOut (programString program)

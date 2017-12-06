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

exception Conversion of string

datatype binaryOperator =
    BOP_PLUS
  | BOP_MINUS
  | BOP_TIMES
  | BOP_DIVIDE
  | BOP_MOD
  | BOP_EQ
  | BOP_NE
  | BOP_LT
  | BOP_GT
  | BOP_LE
  | BOP_GE
  | BOP_AND
  | BOP_OR
  | BOP_COMMA

datatype unaryOperator =
    UOP_NOT
  | UOP_TYPEOF
  | UOP_MINUS

datatype expression =
    EXP_NUM of int
  | EXP_STRING of string
  | EXP_ID of string
  | EXP_FUNCTION of {id: expression, params: expression list, body: sourceElement list}
  | EXP_CALL of {func: expression, args: expression list}
  | EXP_TRUE
  | EXP_FALSE
  | EXP_UNDEFINED
  | EXP_THIS
  | EXP_PROP of string
  | EXP_MEMBER of {obj: expression, prop: expression}
  | EXP_BINARY of {opr: binaryOperator, lft: expression, rht: expression}
  | EXP_UNARY of {opr: unaryOperator, opnd: expression}
  | EXP_COND of {guard: expression, thenExpr: expression, elseExpr: expression}
  | EXP_ASSIGN of {lhs: expression, rhs: expression}
  | EXP_OBJ_LIT of {props: (string * expression) list}
  | EXP_OBJ_CONSTR of {constr: expression, args: expression list}

and variable = 
    VAR_DEC of {id: expression}
  | VAR_INIT of {id: expression, expr: expression}

and statement =
    ST_EXP of {expr: expression}
  | BLOCK_ST of {stmts: statement list}
  | IF_ST of {guard: expression, thenBlock: statement, elseBlock: statement}
  | PRINT_ST of {expr: expression}
  | WHILE_ST of {guard: expression, block: statement}
  | RETURN_ST of {expr: expression}

and sourceElement = 
    STMT of {stmt: statement}
  | FN_DEC of expression
  | VAR_ELEM of {vars: variable list}

datatype program = PROGRAM of {elems: sourceElement list}

fun varId (VAR_DEC {id}) = id
  | varId (VAR_INIT {id, expr}) = id

val binaryOpTokens = [
    (TK_PLUS, BOP_PLUS),
    (TK_MINUS, BOP_MINUS),
    (TK_TIMES, BOP_TIMES),
    (TK_DIVIDE, BOP_DIVIDE),
    (TK_MOD, BOP_MOD),
    (TK_EQ, BOP_EQ),
    (TK_NE, BOP_NE),
    (TK_LT, BOP_LT),
    (TK_GT, BOP_GT),
    (TK_LE, BOP_LE),
    (TK_GE, BOP_GE),
    (TK_AND, BOP_AND),
    (TK_OR, BOP_OR),
    (TK_COMMA, BOP_COMMA)
]

val unaryOpTokens = [
    (TK_NOT, UOP_NOT),
    (TK_TYPEOF, UOP_TYPEOF),
    (TK_MINUS, UOP_MINUS)
]

(* Converts a token to a binary operator. *)
fun tokenToBinaryOp token = (
    case findMatch token binaryOpTokens of
        SOME bop => bop
      | NONE => raise Conversion (tokenToString token)
  )

(* Converts a token to a unary operator. *)
fun tokenToUnaryOp token = (
    case findMatch token unaryOpTokens of
        SOME uop => uop
      | NONE => raise Conversion (tokenToString token)
)

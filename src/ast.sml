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

use "util.sml";

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
   | EXP_CLOSURE of {id: expression, params: expression list, body: sourceElement list}
   | EXP_FN_CALL of {id: expression, args: expression list}
   | EXP_TRUE
   | EXP_FALSE
   | EXP_UNDEFINED
   | EXP_BINARY of {opr: binaryOperator, lft: expression, rht: expression}
   | EXP_UNARY of {opr: unaryOperator, opnd: expression}
   | EXP_COND of {guard: expression, thenExpr: expression, elseExpr: expression}
   | EXP_ASSIGN of {lhs: expression, rhs: expression}

and variable = VAR_DEC of {id: expression, value: expression}

and statement =
    ST_EXP of {expr: expression}
  | BLOCK_ST of {stmts: statement list}
  | IF_ST of {guard: expression, thenBlock: statement, elseBlock: statement}
  | PRINT_ST of {expr: expression}
  | WHILE_ST of {guard: expression, block: statement}
  | RETURN_ST of {value: expression}

and sourceElement = 
    STMT of {stmt: statement}
  | FN_DEC of expression
  | VAR_ELEM of {vars: variable list}

datatype program = PROGRAM of {elems: sourceElement list}

(* Raises a conversion error for the given token. *)
fun conversionError token =
    let
        val tknStr = tokenToString token
        val errMsg = "invalid operator '" ^ tknStr ^ "'"
    in
        raise ConversionError (errMsg)
    end

(* Converts a token to a binary operator. *)
fun tknToBop TK_PLUS = BOP_PLUS
  | tknToBop TK_MINUS = BOP_MINUS
  | tknToBop TK_TIMES = BOP_TIMES
  | tknToBop TK_DIVIDE = BOP_DIVIDE
  | tknToBop TK_MOD = BOP_MOD
  | tknToBop TK_EQ = BOP_EQ
  | tknToBop TK_NE = BOP_NE
  | tknToBop TK_LT = BOP_LT
  | tknToBop TK_GT = BOP_GT
  | tknToBop TK_LE = BOP_LE
  | tknToBop TK_GE = BOP_GE
  | tknToBop TK_AND = BOP_AND
  | tknToBop TK_OR = BOP_OR
  | tknToBop TK_COMMA = BOP_COMMA
  | tknToBop other = conversionError other

(* Converts a token to a unary operator. *)
fun tknToUop TK_NOT = UOP_NOT
  | tknToUop TK_TYPEOF = UOP_TYPEOF
  | tknToUop TK_MINUS = UOP_MINUS
  | tknToUop other = conversionError other

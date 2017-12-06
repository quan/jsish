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

exception InvalidSymbol of string
exception NotKeyword of string

(* The tokens used in the jsish language. *)
datatype token = 
    TK_NUM of int
  | TK_ID of string
  | TK_STRING of string
  | TK_TRUE
  | TK_FALSE
  (* Keywords *)
  | TK_ELSE
  | TK_FUNCTION
  | TK_IF
  | TK_NEW
  | TK_PRINT
  | TK_RETURN
  | TK_THIS
  | TK_TYPEOF
  | TK_UNDEFINED
  | TK_VAR
  | TK_WHILE
  | TK_LBRACE
  | TK_RBRACE
  | TK_LPAREN
  | TK_RPAREN
  | TK_LBRACKET
  | TK_RBRACKET
  | TK_COMMA
  | TK_SEMI
  | TK_QUESTION
  | TK_COLON
  | TK_DOT
  (* Operations *)
  | TK_ASSIGN
  | TK_AND
  | TK_OR
  | TK_NOT
  | TK_EQ
  | TK_NE
  | TK_LT
  | TK_GT
  | TK_LE
  | TK_GE
  | TK_PLUS
  | TK_MINUS
  | TK_TIMES
  | TK_DIVIDE
  | TK_MOD
  (* Other/Error *)
  | TK_EOF

val keywordStrings = [
    (TK_TRUE, "true"),
    (TK_FALSE, "false"),
    (TK_ELSE, "else"),
    (TK_FUNCTION, "function"),
    (TK_IF, "if"),
    (TK_NEW, "new"),
    (TK_PRINT, "print"),
    (TK_RETURN, "return"),
    (TK_THIS, "this"),
    (TK_TYPEOF, "typeof"),
    (TK_UNDEFINED, "undefined"),
    (TK_VAR, "var"),
    (TK_WHILE, "while")
]

val symbolStrings = [
    (TK_LBRACE, "{"),
    (TK_RBRACE, "}"),
    (TK_LPAREN, "("),
    (TK_RPAREN, ")"),
    (TK_LBRACKET, "["),
    (TK_RBRACKET, "]"),
    (TK_COMMA, ","),
    (TK_SEMI, ";"),
    (TK_QUESTION, "?"),
    (TK_COLON, ":"),
    (TK_DOT, "."),
    (TK_ASSIGN, "="),
    (TK_AND, "&&"),
    (TK_OR, "||"),
    (TK_NOT, "!"),
    (TK_EQ, "=="),
    (TK_NE, "!="),
    (TK_LT, "<"),
    (TK_GT, ">"),
    (TK_LE, "<="),
    (TK_GE, ">="),
    (TK_PLUS, "+"),
    (TK_MINUS, "-"),
    (TK_TIMES, "*"),
    (TK_DIVIDE, "/"),
    (TK_MOD, "%"),
    (TK_EOF, "eof")
]

val tokenStrings = List.concat [keywordStrings, symbolStrings]

(* Returns the string representation of a token. *)
fun tokenToString (TK_ID id) = id
  | tokenToString (TK_NUM num) = Int.toString num
  | tokenToString (TK_STRING st) = "string: " ^ st
  | tokenToString token = (
    case findMatch token tokenStrings of
        NONE => raise ConversionError "unrecognized token"
      | SOME s => s)

(* Converts a string to a Symbol Token. *) 
fun stringToSymbol s = (
    case findMatchRev s symbolStrings of
        NONE => raise InvalidSymbol s
      | SOME token => token)

(* Converts a string to a Keyword Token. *)
fun stringToKeyword s = (
    case findMatchRev s keywordStrings of
        NONE => raise NotKeyword s
      | SOME token => token)

(* Converts an Identifier Token to a Keyword Token. *)
fun idToKeyword (TK_ID value) = stringToKeyword value

(* Checks if the given token is a keyword in jsish. *)
fun isKeyword (TK_ID value) = let val keyword = stringToKeyword value in true end
    handle NotKeyword _ => false

(* Checks if the given token is a unary operator. *)
fun isUnaryOp TK_NOT = true
  | isUnaryOp TK_TYPEOF = true
  | isUnaryOp TK_MINUS = true
  | isUnaryOp _ = false

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

exception UnwrapMismatch of (string * string)
exception InvalidType of string
exception FunctionInvocation of string

(* The data types available in jsish. *)
datatype value =
    NUMBER of int
  | STRING of string
  | BOOL of bool
  | CLOSURE of {
        id: unit ref,
        scope: (string, value) HashTable.hash_table list,
        params: expression list,
        body: sourceElement list
    }
  | REFERENCE of int
  | UNDEFINED
  | RETURN of value

fun typeError expected found oper =
    raise InvalidType ("operator " ^ sq oper ^ " requires " ^ expected ^ ", found " ^ found)

fun unaryInvalidType expected found oper =
    raise InvalidType ("unary operator " ^ sq oper ^ " requires " ^ expected ^ ", found " ^ found)

(* Returns the type of the give jsish value as a string. *)
fun typeof (NUMBER _) = "number"
  | typeof (STRING _) = "string"
  | typeof (BOOL _) = "boolean"
  | typeof (CLOSURE _) = "function"
  | typeof (REFERENCE _) = "object"
  | typeof UNDEFINED = "undefined"
  | typeof (RETURN _) = "return value"

(* Converts a jsish value to a string. *)
fun valToString (NUMBER num) = intToString num
  | valToString (STRING st) = st
  | valToString (BOOL bln) = boolToString bln
  | valToString (CLOSURE _) = "function"
  | valToString (REFERENCE _) = "object"
  | valToString UNDEFINED = "undefined"
  | valToString (RETURN value) = "return " ^ valToString value

(* Check the type of jsish values. *)
fun isNum (NUMBER _) = true | isNum _ = false
fun isStr (STRING _) = true | isStr _ = false
fun isBool (BOOL _) = true | isBool _ = false
fun isFunc (CLOSURE _) = true | isFunc _ = false
fun isObj (REFERENCE _) = true | isObj _ = false
fun isUndef UNDEFINED = true | isUndef _ = false

(* Extract encapsulated values of jsish values. *)
fun functionScope (CLOSURE {id, scope, params, body}) = scope
  | functionScope value = raise FunctionInvocation (typeof value)
fun functionBody (CLOSURE {id, scope, params, body}) = body
  | functionBody value = raise FunctionInvocation (typeof value)
fun functionParams (CLOSURE {id, scope, params, body}) = params
  | functionParams value = raise FunctionInvocation (typeof value)
fun addressOf (REFERENCE addr) = addr
  | addressOf other = raise InvalidType ("field reference '.' requires object, found " ^ typeof other)

(* Unwrap jsish values, returning their SML equivalents. *)
fun unwrapNum (NUMBER num) = num
  | unwrapNum other = raise UnwrapMismatch ("number", typeof other)
fun unwrapStr (STRING st) = st
  | unwrapStr other = raise UnwrapMismatch ("string", typeof other)
fun unwrapBool (BOOL bln) = bln
  | unwrapBool other = raise UnwrapMismatch ("boolean", typeof other)
fun unwrapReturn (RETURN value) = value
  | unwrapReturn other = raise UnwrapMismatch ("return value", typeof other)

(* Adds two jsish values together. Concatenates strings. *)
fun addVals (NUMBER n1) (NUMBER n2) = NUMBER (n1 + n2)
  | addVals (STRING s1) (STRING s2) = STRING (s1 ^ s2)
  | addVals lft rht = raise InvalidType (
    "operator '+' requires number * number or string * string, " ^
    "found " ^ typeof lft  ^ " * " ^ typeof rht)

(* Checks if two jsish values are equal. *)
fun areValsEq (NUMBER lft) (NUMBER rht) = BOOL (lft = rht)
  | areValsEq (STRING lft) (STRING rht) = BOOL (lft = rht)
  | areValsEq (BOOL lft) (BOOL rht) = BOOL (lft = rht)
  | areValsEq (CLOSURE lft) (CLOSURE rht) = BOOL (#id lft = #id rht)
  | areValsEq (REFERENCE lft) (REFERENCE rht) = BOOL (lft = rht)
  | areValsEq UNDEFINED UNDEFINED = BOOL true
  | areValsEq _ _ = BOOL false

(* Checks if two jsish values are not equal. *)
fun areValsNotEq first second = BOOL (not (unwrapBool (areValsEq first second)))

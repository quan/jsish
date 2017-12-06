use "util.sml";

(* Datatype representing a jsish value.
   A jsish value can be one of five types:
   - a number
   - a string
   - a boolean
   - a function
   - undefined
*)
datatype value =
    NUMBER of int
  | STRING of string
  | BOOL of bool
  | FUNCTION of {id: unit ref, scope: (string, value) HashTable.hash_table list, params: expression list, body: sourceElement list}
  | UNDEFINED
  | RETURN of value

(* UnwrapMismatch constructed with expected type and found type. *)
exception UnwrapMismatch of (string * string)
exception InvalidType of string
exception FunctionInvocation of string

(* Raises a type error for an operation applied to a invalid type. *)
fun typeError expected found oper =
    let
        val msg = "operator '" ^ oper ^ "' requires " ^ expected ^ ", found " ^ found
    in
        raise InvalidType (msg)
    end

(* Raises a type error for a unary operation applied to an invalid type. *)
fun unaryInvalidType expected found oper =
    let
        val msg = "unary operator '" ^ oper ^ "' requires " ^ expected ^ ", found " ^ found
    in
        raise InvalidType (msg)
    end

(* Converts a jsish value to a string. *)
fun valToString (NUMBER num) = intToString num
  | valToString (STRING st) = st
  | valToString (BOOL bln) = boolToString bln
  | valToString (FUNCTION {id, scope, params, body}) = "function"
  | valToString UNDEFINED = "undefined"

(* Checks if a jsish value is a number. *)
fun isNum (NUMBER _) = true | isNum _ = false

(* Checks if a jsish value is a string. *)
fun isStr (STRING _) = true | isStr _ = false

(* Checks if a jsish value is a boolean. *)
fun isBool (BOOL _) = true | isBool _ = false

(* Checks if a jsish value is a function. *)
fun isFunc (FUNCTION _) = true | isFunc _ = false  

(* Checks if a jsish value is undefined. *)
fun isUndef UNDEFINED = true | isUndef _ = false

(* Returns the type of the give jsish value as a string. *)
fun typeof (NUMBER _) = "number"
  | typeof (STRING _) = "string"
  | typeof (BOOL _) = "boolean"
  | typeof (FUNCTION _) = "function"
  | typeof UNDEFINED = "undefined"

(* Unwraps a jsish number value. *)
fun unwrapNum (NUMBER num) = num
  | unwrapNum other = raise UnwrapMismatch ("number", typeof other)

(* Unwraps a jsish string value. *)
fun unwrapStr (STRING st) = st
  | unwrapStr other = raise UnwrapMismatch ("string", typeof other)

(* Unwraps a jsish boolean value. *)
fun unwrapBool (BOOL bln) = bln
  | unwrapBool other = raise UnwrapMismatch ("boolean", typeof other)

(* Unwraps a jsish return value. *)
fun unwrapReturn (RETURN value) = value
  | unwrapReturn other = raise UnwrapMismatch ("return value", typeof other)

(* Adds two jsish values together. Concatenates strings. *)
fun addVals (NUMBER n1) (NUMBER n2) = NUMBER (n1 + n2)
  | addVals (STRING s1) (STRING s2) = STRING (s1 ^ s2)
  | addVals first second =
    let
        val typeL = typeof first  
        val typeR = typeof second
        val msg = "operator '+' requires number * number or string * string, found " ^ typeL ^ " * " ^ typeR
    in
        raise InvalidType (msg)
    end

(* Checks if two jsish values are equal. *)
fun areValsEq (NUMBER n1) (NUMBER n2) = BOOL (n1 = n2)
  | areValsEq (STRING s1) (STRING s2) = BOOL (s1 = s2)
  | areValsEq (BOOL b1) (BOOL b2) = BOOL (b1 = b2)
  | areValsEq (FUNCTION {id=id1, scope=scope1, params=params1, body=body1})
              (FUNCTION {id=id2, scope=scope2, params=params2, body=body2}) = 
    BOOL (id1 = id2)
  | areValsEq UNDEFINED UNDEFINED = BOOL (true)
  | areValsEq _ _ = BOOL (false)

(* Checks if two jsish values are not equal. *)
fun areValsNotEq first second = BOOL (not (unwrapBool (areValsEq first second)))

fun functionScope (FUNCTION {id, scope, params, body}) = scope
  | functionScope value = raise FunctionInvocation (typeof value)

(* Extracts the body of a function value. *)
fun functionBody (FUNCTION {id, scope, params, body}) = body
  | functionBody value = raise FunctionInvocation (typeof value)

fun functionParams (FUNCTION {id, scope, params, body}) = params
  | functionParams value = raise FunctionInvocation (typeof value)

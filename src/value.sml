use "util.sml";

(* Datatype representing a jsish value.
   A jsish value can be one of four types:
   - a number
   - a string
   - a boolean
   - undefined
*)
datatype value =
    NUMBER of int
  | STRING of string
  | BOOL of bool
  | FUNCTION of string
  | UNDEFINED

(* UnwrapMismatch constructed with expected type and found type. *)
exception UnwrapMismatch of (string * string)
exception InvalidType of string

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
  | valToString (FUNCTION func) = "function"
  | valToString UNDEFINED = "undefined"

(* Checks if a jsish value is a number. *)
fun isNum (NUMBER num) = true | isNum other = false

(* Checks if a jsish value is a string. *)
fun isStr (STRING st) = true | isStr other = false

(* Checks if a jsish value is a boolean. *)
fun isBool (BOOL bln) = true | isBool other = false

(* Checks if a jsish value is undefined. *)
fun isUndef UNDEFINED = true | isUndef other = false

(* Returns the type of the give jsish value as a string. *)
fun typeof (NUMBER num) = "number"
  | typeof (STRING st) = "string"
  | typeof (BOOL bln) = "boolean"
  | typeof UNDEFINED = "undefined"

(* UnwrapMismatchs a jsish number value. *)
fun unwrapNum (NUMBER num) = num
  | unwrapNum other = raise UnwrapMismatch ("number", typeof other)

(* UnwrapMismatchs a jsish string value. *)
fun unwrapStr (STRING st) = st
  | unwrapStr other = raise UnwrapMismatch ("string", typeof other)

(* UnwrapMismatchs a jsish boolean value. *)
fun unwrapBool (BOOL bln) = bln
  | unwrapBool other = raise UnwrapMismatch ("boolean", typeof other)

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
  | areValsEq UNDEFINED UNDEFINED = BOOL (true)
  | areValsEq _ _ = BOOL (false)

(* Checks if two jsish values are not equal. *)
fun areValsNotEq first second = BOOL (
    not (unwrapBool (areValsEq first second))
)
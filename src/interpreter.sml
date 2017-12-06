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
use "value.sml";
use "env.sml";

exception Return
exception InvalidIdentifer of string
exception UnaryOperation of (string * string * string)
exception BinaryOperation of (string * string * string)

(*  Dies from a type error from a binary operation for the given type raised by
    the two given expressions. *)
fun binaryTypeError state reqType lft rht oper =
    let
        val typeL = typeof (evalExpr state lft)
        val typeR = typeof (evalExpr state rht)
        val expected = reqType ^ " * " ^ reqType
        val found = typeL ^ " * " ^ typeR
    in
        raise BinaryOperation (expected, found, oper)
    end

(*  Applies the given mathematical operation to the two number expressions.
    Takes the operator as a string for error messages. *)
and appMathOp state f oper lft rht = 
    let
        val first = unwrapNum (evalExpr state lft)
        val second = unwrapNum (evalExpr state rht)
    in
        NUMBER (f (first, second))
    end
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht oper

(*  Applies the given relational operation to the two number expressions.
    Takes the operator as a string for error messages. *)
and appRelOp state f oper lft rht =
    let
        val first = unwrapNum (evalExpr state lft)
        val second = unwrapNum (evalExpr state rht)
    in
        BOOL (f (first, second))
    end
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht oper

(*  Evaluates the two given expressions and applies the given function to the
    resulting jsish values. *)
and evalOp state oper lft rht =
    let
        val first = evalExpr state lft
        val second = evalExpr state rht
    in
        oper first second
    end

(*  Evaluates a jsish logical AND operation for the given expressions. *)
and evalAnd state lft rht = BOOL (
    unwrapBool (evalExpr state lft) andalso unwrapBool (evalExpr state rht)
) handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "&&"

(*  Evaluates a jsish logical OR operation for the given expressions. *)
and evalOr state lft rht = BOOL (
    unwrapBool (evalExpr state lft) orelse unwrapBool (evalExpr state rht)
) handle UnwrapMismatch (expected, found) => raise BinaryOperation ("boolean", typeof (evalExpr state lft), "||")

(*  Evaluates a jsish binary operation with the given operator and expressions. *)
and evalBinaryExpr state BOP_PLUS lft rht = evalOp state addVals lft rht
  | evalBinaryExpr state BOP_MINUS lft rht = appMathOp state (op -) "-" lft rht
  | evalBinaryExpr state BOP_TIMES lft rht = appMathOp state (op * ) "*" lft rht
  | evalBinaryExpr state BOP_DIVIDE lft rht = appMathOp state (op div) "/" lft rht
  | evalBinaryExpr state BOP_MOD lft rht = appMathOp state (op mod) "%" lft rht
  | evalBinaryExpr state BOP_EQ lft rht = evalOp state areValsEq lft rht
  | evalBinaryExpr state BOP_NE lft rht = evalOp state areValsNotEq lft rht
  | evalBinaryExpr state BOP_LT lft rht = appRelOp state (op <) "<" lft rht
  | evalBinaryExpr state BOP_GT lft rht = appRelOp state (op >) ">" lft rht
  | evalBinaryExpr state BOP_LE lft rht = appRelOp state (op <=) "<=" lft rht
  | evalBinaryExpr state BOP_GE lft rht = appRelOp state (op >=) ">=" lft rht
  | evalBinaryExpr state BOP_AND lft rht = evalAnd state lft rht
  | evalBinaryExpr state BOP_OR lft rht = evalOr state lft rht
  | evalBinaryExpr state BOP_COMMA lft rht = (evalExpr state lft; evalExpr state rht)

(*  Evaluates a jsish unary operation with the given operator and expression. *)
and evalUnaryExpr state UOP_TYPEOF expr = STRING (typeof (evalExpr state expr))
  | evalUnaryExpr state UOP_NOT expr = (BOOL (not (unwrapBool (evalExpr state expr)))
    handle UnwrapMismatch (expected, found) => raise UnaryOperation (expected, found, "!"))
  | evalUnaryExpr state UOP_MINUS expr = (NUMBER (~ (unwrapNum (evalExpr state expr)))
    handle UnwrapMismatch (expected, found) => raise UnaryOperation (expected, found, "-"))

(*  Evaluates a jsish assignment, assigning the value of the rhs to the lhs id. *)
and evalAssignment state (EXP_ID id) rhs = bindValue state (evalExpr state rhs) id
  | evalAssignment state other rhs = raise LeftHandSide ("broken lhs error")

(*  Evaluates a jsish conditional with the given condition and branches. *)
and evalCondExpr state guard thenExpr elseExpr =
    let
        val cond = evalExpr state guard
    in
        if unwrapBool cond
        then evalExpr state thenExpr
        else evalExpr state elseExpr
    end
    handle UnwrapMismatch (expected, found) =>
    raise InvalidType ("boolean guard required for 'cond' expression, found " ^ found)

(* Binds an argument to a parameter in the current environment. *)
and assignParam state arg (EXP_ID id) = (bindValueInCurrentEnv state arg id; ())
  | assignParam state arg other = raise LeftHandSide ("broken lhs error")
 
(* Assigns function call arguments to parameters. *)
and assignParams state [] args = ()
  | assignParams state params [] = app (assignParam state UNDEFINED) params
  | assignParams state (param::params) (arg::args) =
    (assignParam state arg param; assignParams state params args)

(* Evaluates a statement that may be a return statement in a function. *)
and evalFunctionStatement state (RETURN_ST {expr}) = RETURN (evalExpr state expr)
  | evalFunctionStatement state (BLOCK_ST {stmts}) = (
    let
        fun evalFunctionBlock state [] = UNDEFINED
          | evalFunctionBlock state (RETURN_ST {expr}::stmts) = RETURN (evalExpr state expr)
          | evalFunctionBlock state (stmt::stmts) = (
            case evalFunctionStatement state stmt of
                RETURN value => RETURN value
              | _ => evalFunctionBlock state stmts)
    in
        evalFunctionBlock state stmts
    end)
  | evalFunctionStatement state (WHILE_ST {guard, block}) = (
    let
        fun evalFunctionWhileStatement state guard block =
            if unwrapBool (evalExpr state guard)
            then (
                case evalFunctionStatement state block of
                    RETURN value => RETURN value
                  | _ => evalFunctionWhileStatement state guard block)
            else UNDEFINED
    in
        evalFunctionWhileStatement state guard block
    end
    handle UnwrapMismatch (expected, found) => 
    raise InvalidType ("boolean guard required for 'while' statement, found " ^ found))
  | evalFunctionStatement state (IF_ST {guard, thenBlock, elseBlock}) = ((
    if unwrapBool (evalExpr state guard)
    then evalFunctionStatement state thenBlock
    else evalFunctionStatement state elseBlock)
    handle UnwrapMismatch (expected, found) => 
    raise InvalidType ("boolean guard required for 'if' statement, found " ^ found))
  | evalFunctionStatement state stmt = (evalStatement state stmt; UNDEFINED)

(* Evaluates the body of a function, exiting when a return value is encountered. *)
and evalFunctionBody state [] = RETURN UNDEFINED
  | evalFunctionBody state (STMT {stmt}::elems) = (
    case evalFunctionStatement state stmt of
        RETURN value => RETURN value
      | _ => evalFunctionBody state elems)
  | evalFunctionBody state (elem::elems) = (evalSrcElem state elem; evalFunctionBody state elems)

(* Evaluates a function call. *)
and evalFunctionCall state expr argList =
    let
        val function = (case expr of
            EXP_ID id => (lookupValue state id handle NotFound => raise InvalidIdentifer id)
          | functionExpr => evalExpr state functionExpr)
        (* Evaluate the arguments before pushing a new environment scope. *)
        val args = map (evalExpr state) argList
        (* Create a new environment scope for the function. *)
        val newState = newEnv (functionScope function)
    in (
        (*printLn "hhhhhhhhhhhhhhhhhhhh";*)
        assignParams newState (functionParams function) args;
        unwrapReturn (evalFunctionBody newState (functionBody function)))
    end


(*  Evaluates an expression to a jsish value. *)
and evalExpr state EXP_TRUE = BOOL true
  | evalExpr state EXP_FALSE = BOOL false
  | evalExpr state EXP_UNDEFINED = UNDEFINED
  | evalExpr state (EXP_FUNCTION {id, params, body}) = 
    let
        val midScope = newEnv state
        val function = FUNCTION {id=ref(), scope=midScope, params=params, body=body}
    in
        (case id of
            EXP_ID name => (bindValueInCurrentEnv midScope function name; function)
          | _ => function)
    end
  | evalExpr state (EXP_CALL {func, args}) = evalFunctionCall state func args
  | evalExpr state (EXP_NUM num) = NUMBER num
  | evalExpr state (EXP_STRING st) = STRING st
  | evalExpr state (EXP_BINARY {opr, lft, rht}) = evalBinaryExpr state opr lft rht
  | evalExpr state (EXP_UNARY {opr, opnd}) = evalUnaryExpr state opr opnd
  | evalExpr state (EXP_ASSIGN {lhs, rhs}) = evalAssignment state lhs rhs
  | evalExpr state (EXP_COND {guard, thenExpr, elseExpr}) =
    evalCondExpr state guard thenExpr elseExpr
  | evalExpr state (EXP_ID id) = (lookupValue state id)
    handle NotFound => raise InvalidIdentifer id

(*  Evaluates a jsish statement. *)
and evalStatement state (ST_EXP {expr}) = (evalExpr state expr; ())
  | evalStatement state (BLOCK_ST {stmts}) = app (evalStatement state) stmts
  | evalStatement state (PRINT_ST {expr}) = printOut (valToString (evalExpr state expr))
  | evalStatement state (RETURN_ST {expr}) = raise Return
  | evalStatement state (IF_ST {guard, thenBlock, elseBlock}) = ((
    if unwrapBool (evalExpr state guard)
    then evalStatement state thenBlock
    else evalStatement state elseBlock; ())
    handle UnwrapMismatch (expected, found) => 
    raise InvalidType ("boolean guard required for 'if' statement, found " ^ found))
  | evalStatement state (WHILE_ST {guard, block}) =
    let
        fun evalWhileStatement state guard block =
            if unwrapBool (evalExpr state guard)
            then (
                evalStatement state block;
                evalWhileStatement state guard block)
            else ()
    in
        evalWhileStatement state guard block
    end
    handle UnwrapMismatch (expected, found) => 
    raise InvalidType ("boolean guard required for 'while' statement, found " ^ found)

(*  Evaluates a jsih function declaration, creating a function value for a 
    closure and assigning it to an identifier. *)
and evalFunctionDec state (EXP_FUNCTION {id=EXP_ID id, params, body}) =
    let
        val currentEnv = hd state
        val function = FUNCTION {id=ref(), scope=state, params=params, body=body}
    in
        (bindValueInEnv currentEnv function id; ())
    end

(*  Evaluates a jsish variable declaration, binding a value to an identifier in
    the current environment. *)
and evalVariableDec state (VAR_DEC {id=EXP_ID id}) =
    if not (definedInCurrentEnv state id)
    then (bindValueInCurrentEnv state UNDEFINED id; ())
    else ()
  | evalVariableDec state (VAR_INIT {id=EXP_ID id, expr}) =
    (bindValueInCurrentEnv state (evalExpr state expr) id; ())

(*  Evaluates a jsish source element. *)
and evalSrcElem state (STMT {stmt}) = evalStatement state stmt
  | evalSrcElem state (FN_DEC expression) = evalFunctionDec state expression
  | evalSrcElem state (VAR_ELEM {vars}) = app (evalVariableDec state) vars

(*  Evaluates a jsish program. *)
fun evalProgram state (PROGRAM {elems}) = app (evalSrcElem state) elems

(*  Interpret the jsish program contained in the given filename. *)
fun interpret filename = 
    let
        (* Create a hash table to represent the state. *)
        val envs = newEnv []
    in
        evalProgram envs (parse filename)
    end
    handle InvalidType msg => error msg
         | Return => error "return statements are only valid inside functions"
         | InvalidIdentifer id => error ("variable '" ^ id ^ "' not found")
         | UnaryOperation (expected, found, oper) => error ("unary operator '" ^ oper ^ "' requires " ^ expected ^ ", found " ^ found)
         | BinaryOperation (expected, found, oper) => error ("operator '" ^ oper ^ "' requires " ^ expected ^ ", found " ^ found)
         | FunctionInvocation value => error ("attempt to invoke '" ^ value ^ "' value as a function")
         | LeftHandSide expr => error ("invalid left-hand side expression '" ^ expr ^ "'")

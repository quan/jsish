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
use "object.sml";

exception This
exception Return
exception DivideByZero
exception FunctionDeclaration of string
exception VariableIdentifier of string
exception IfGuard of string
exception TernaryGuard of string
exception WhileGuard of string
exception NewOperand of string
exception UnaryOperation of (string * string * string)
exception BinaryOperation of (string * string * string)

(*  Looks up an identifier in the state. *)
fun resolve id state = lookup (stateEnvs state) id 
    handle NotFound => raise InvalidIdentifier id

fun exprToString state = unwrapStr o evalExpr state
and exprToNum state = unwrapNum o evalExpr state
and exprToBool state = unwrapBool o evalExpr state
and refToObj state = deref state o addressOf
and exprToObj state = refToObj state o evalExpr state
and exprType state (EXP_OBJ_CONSTR _) = "object"
  | exprType state expr = (typeof o evalExpr state) expr

(*  Creates a function value, creating a new encapsulating state with a new environment 
    closure for named, self-referencing function expressions.
    Returns the new environment and the new function. *)
and createClosure state params body =
    let
        val scope = stateEnvs state
        val newState = pushEnv state scope
    in
        (CLOSURE {id=ref (), scope=stateEnvs newState, params=params, body=body}, newState)
    end

(*  Evaluates a property assignment, evaluating the assigned expression and returning a 
    tuple with the property name and the value. *)
and evalPropAssignment state (prop, expr) = (prop, evalExpr state expr)

(*  Dies from a type error from a binary operation for the given type raised by
    the two given expressions. *)
and binaryTypeError state required lft rht oper = raise BinaryOperation (
    required ^ " * " ^ required,
    (exprType state lft) ^ " * " ^ (exprType state rht),
    oper
)
and appMathOp state f lft rht = NUMBER (f (exprToNum state lft, exprToNum state rht))

and appDivOp state lft rht =
    let
        val divisor = exprToNum state rht
    in
        if divisor = 0
        then raise DivideByZero
        else NUMBER (exprToNum state lft div divisor)
    end

and appRelOp state f lft rht = BOOL (f (exprToNum state lft, exprToNum state rht))

and evalOp state f lft rht = f (evalExpr state lft) (evalExpr state rht)

and evalAnd state lft rht = BOOL (exprToBool state lft andalso exprToBool state rht)
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "&&"

and evalOr state lft rht = BOOL (exprToBool state lft orelse exprToBool state rht)
    handle UnwrapMismatch (expected, found) => raise BinaryOperation ("boolean", typeof (evalExpr state lft), "||")

(*  Evaluates a jsish binary operation that may result in returning an object.
    Pushes objects onto the state's pending list to prevent premature garbage collection. *)
and evalObjOp state f lft rht =
    let
        val left = evalExpr state lft
        val newState = pendingState state left
        val right = evalExpr newState rht
    in
        f left right
    end

(*  Evaluates access of an object member. *)
and evalMemberAccess state lft rht =
    let
        val obj = exprToObj state lft
        val prop = exprToString state rht
    in
        if not (defined (objProps obj) prop)
        then UNDEFINED
        else getProp obj prop
    end

(*  Evaluates a jsish binary operation with the given operator and expressions. *)
and evalBinaryExpr state BOP_PLUS lft rht = evalOp state addVals lft rht
  | evalBinaryExpr state BOP_MINUS lft rht = (appMathOp state (op -) lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "-")
  | evalBinaryExpr state BOP_TIMES lft rht = (appMathOp state (op * ) lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "*")
  | evalBinaryExpr state BOP_DIVIDE lft rht = (appDivOp state lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "/")
  | evalBinaryExpr state BOP_MOD lft rht = (appMathOp state (op mod) lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "%")
  | evalBinaryExpr state BOP_LT lft rht = (appRelOp state (op <) lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "<")
  | evalBinaryExpr state BOP_GT lft rht = (appRelOp state (op >) lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht ">")
  | evalBinaryExpr state BOP_LE lft rht = (appRelOp state (op <=) lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht "<=")
  | evalBinaryExpr state BOP_GE lft rht = (appRelOp state (op >=) lft rht
    handle UnwrapMismatch (expected, found) => binaryTypeError state expected lft rht ">=")
  | evalBinaryExpr state BOP_EQ lft rht = evalObjOp state areValsEq lft rht
  | evalBinaryExpr state BOP_NE lft rht = evalObjOp state areValsNotEq lft rht
  | evalBinaryExpr state BOP_AND lft rht = evalAnd state lft rht
  | evalBinaryExpr state BOP_OR lft rht = evalOr state lft rht
  | evalBinaryExpr state BOP_COMMA lft rht = (evalExpr state lft; evalExpr state rht)

(*  Evaluates a jsish unary operation with the given operator and expression. *)
and evalUnaryExpr state UOP_TYPEOF expr = STRING (exprType state expr)
  | evalUnaryExpr state UOP_NOT expr = (BOOL ((not o exprToBool state) expr)
    handle UnwrapMismatch (expected, found) => raise UnaryOperation (expected, found, "!"))
  | evalUnaryExpr state UOP_MINUS expr = (NUMBER ((~ o exprToNum state) expr)
    handle UnwrapMismatch (expected, found) => raise UnaryOperation (expected, found, "-"))

(*  Evaluates a jsish assignment, assigning the value of the rhs to the lhs id. *)
and evalAssignment state (EXP_ID id) rhs = 
    let
        val value = evalExpr state rhs
    in
        assign state value id;
        value
    end
(* Evaluates an assignment to an object property. *)
  | evalAssignment state (EXP_MEMBER {obj, prop}) rhs =
    let
        val rhsVal = evalExpr state rhs
    in
        setProp (exprToObj state obj) (exprToString state prop, rhsVal);
        rhsVal
    end
  | evalAssignment state other rhs = raise LeftHandSide ("LHS ??")

(*  Evaluates a jsish conditional with the given condition and branches. *)
and evalCondExpr state guard thenExpr elseExpr = (
    if exprToBool state guard
    then evalExpr state thenExpr
    else evalExpr state elseExpr
)   handle UnwrapMismatch (expected, found) => raise TernaryGuard found

(* Binds an argument to a parameter in the current stateironment. *)
and assignParam state arg (EXP_ID id) = bindValueInCurrentEnv state arg id
  | assignParam state arg other = raise LeftHandSide ("LHS ??")
 
(* Assigns function call arguments to parameters. *)
and assignParams state [] args = ()
  | assignParams state params [] = app (assignParam state UNDEFINED) params
  | assignParams state (param::params) (arg::args) =
    (assignParam state arg param; assignParams state params args)

(* Evaluates a function expression, creating a closure and returning it. *)
and evalFunctionExpr state id params body =
    let
        val (function, newState) = createClosure state params body
        (*val *)
    in
        (* Bind a named function expression to an id in its own environment. *)
        case id of
            EXP_ID name => (bindValueInCurrentEnv newState function name; function)
          | _ => function
    end

(*  Evaluates a list of arguments, saving pending evaluated objects to the state to prevent 
    premature garbage collection. *)
and evalArgs state [] = []
  | evalArgs state (arg::args) = 
    let
        val argValue = evalExpr state arg
        val newState = pendingState state argValue
    in
        argValue::(evalArgs newState args)
    end


(* Evaluates a statement in a function, terminating execution if a return statement is
   encountered and returns the wrapped value. *)
(* This was not one of my better ideas. *)
and evalFunctionStatement state (RETURN_ST {expr}) = RETURN (evalExpr state expr)
  | evalFunctionStatement state (BLOCK_ST {stmts}) =
    evalBlockStatement evalFunctionStatement state stmts
  | evalFunctionStatement state (WHILE_ST {guard, block}) = 
    evalWhileStatement evalFunctionStatement state guard block 
  | evalFunctionStatement state (IF_ST {guard, thenBlock, elseBlock}) =
    evalIfStatement evalFunctionStatement state guard thenBlock elseBlock
  | evalFunctionStatement state stmt = evalStatement state stmt

(* Evaluates the body of a function, exiting when a return value is encountered. *)
and evalFunctionBody state [] = RETURN UNDEFINED
  | evalFunctionBody state (STMT {stmt}::elems) = (
    case evalFunctionStatement state stmt of
        RETURN value => RETURN value
      | _ => evalFunctionBody state elems)
  | evalFunctionBody state (elem::elems) = (evalSrcElem state elem; evalFunctionBody state elems)

(*  Evaluates a function call. *)
and evalFunctionCall state expr argList =
    let
        val function = (case expr of
            EXP_ID id => resolve id state
          | EXP_FUNCTION _ => evalExpr state expr
          | EXP_BINARY _ => evalExpr state expr
          | EXP_CALL _ => evalExpr state expr
          | EXP_COND _ => evalExpr state expr
          | EXP_ASSIGN _ => evalExpr state expr
          | other => raise FunctionInvocation (exprType state other))
        val midState = pendingState state function

        val args = evalArgs midState argList
        val newState = pushEnv midState (functionScope function)
    in
        (* This is not a method call, so bind the global object to "this". *)
        bindValueInCurrentEnv newState (REFERENCE 0) "this";
        assignParams newState (functionParams function) args;
        (unwrapReturn o evalFunctionBody newState) (functionBody function)
    end

(*  Evaluates a method call of an object. *)
and evalMethodCall state lft rht argList =
    let
        val this = evalExpr state lft
        val obj = refToObj state this
        val midState = pendingState state this

        val method = getProp obj (exprToString midState rht)
        val args = evalArgs midState argList
        val newState = pushEnv midState (functionScope method)
    in
        bindValueInCurrentEnv newState this "this";
        assignParams newState (functionParams method) args;
        (unwrapReturn o evalFunctionBody newState) (functionBody method)
    end

(* Creates an object literal value with the given props. *)
and createObjectLiteral state props = 
    let
        val obj = newObj ()
        val address = objectAlloc state obj
        val reference = REFERENCE address

        val newState = pushEnv state (stateEnvs state)
    in
        bindValueInCurrentEnv newState reference "this";
        app (setProp obj o evalPropAssignment newState) props;
        reference
    end

(*  Evaluates the construction of a new object. *)
and evalObjectConstr state constr argList =
    let
        val obj = newObj ()
        val address = objectAlloc state obj
        val reference = REFERENCE address

        val midState = pendingState state reference
        val function = evalExpr midState constr
        val args = evalArgs midState argList

        val newState = pushEnv midState (stateEnvs midState)
    in
        bindValueInCurrentEnv newState reference "this";
        assignParams newState (functionParams function) args;
        let
            val return = (evalFunctionBody newState) (functionBody function)
        in
            case return of
                RETURN (REFERENCE _) => unwrapReturn return
              | _ => reference
        end
    end
    handle FunctionInvocation found => raise NewOperand found

(*  Evaluates an expression to a jsish value. *)
and evalExpr state (EXP_NUM num) = NUMBER num
  | evalExpr state (EXP_STRING st) = STRING st
  | evalExpr state (EXP_ID id) = resolve id state
  | evalExpr state (EXP_FUNCTION {id, params, body}) = evalFunctionExpr state id params body
  | evalExpr state (EXP_CALL {func=EXP_MEMBER {obj, prop}, args}) = (
    evalMethodCall state obj prop args)
  | evalExpr state (EXP_CALL {func, args}) = evalFunctionCall state func args
  | evalExpr state EXP_TRUE = BOOL true
  | evalExpr state EXP_FALSE = BOOL false
  | evalExpr state EXP_UNDEFINED = UNDEFINED
  | evalExpr state EXP_THIS = ((resolve "this" state) handle InvalidIdentifier id => raise This)
  | evalExpr state (EXP_PROP prop) = STRING prop (* Return the string for looking up in the object table. *)
  | evalExpr state (EXP_MEMBER {obj, prop}) = evalMemberAccess state obj prop
  | evalExpr state (EXP_BINARY {opr, lft, rht}) = evalBinaryExpr state opr lft rht
  | evalExpr state (EXP_UNARY {opr, opnd}) = evalUnaryExpr state opr opnd
  | evalExpr state (EXP_COND {guard, thenExpr, elseExpr}) = evalCondExpr state guard thenExpr elseExpr
  | evalExpr state (EXP_ASSIGN {lhs, rhs}) = evalAssignment state lhs rhs
  | evalExpr state (EXP_OBJ_LIT {props}) = createObjectLiteral state props
  | evalExpr state (EXP_OBJ_CONSTR {constr, args}) = evalObjectConstr state constr args

(*  Evaluates a block of statements, possibly returning a value. *)
and evalBlockStatement f state [] = UNDEFINED
  | evalBlockStatement f state (stmt::stmts) = (
    case f state stmt of
        RETURN value => RETURN value
      | _ => evalBlockStatement f state stmts)

(*  Evaluates an if statement using the given evaluation function. *)
and evalIfStatement f state guard thenBlock elseBlock = (
    if exprToBool state guard
    then f state thenBlock
    else f state elseBlock
)   handle UnwrapMismatch (expected, found) => raise IfGuard found

and evalPrintStatement state = printOut o valToString o evalExpr state

(*  Evaluates a while statement using the given evaluation function. 
    Returns undefined to accommodate for possible return statements. *)
and evalWhileStatement f state guard block = (
    if exprToBool state guard
    then (case f state block of
        RETURN value => RETURN value
      | _ => evalWhileStatement f state guard block)
    else UNDEFINED
)   handle UnwrapMismatch (expected, found) => raise WhileGuard found

(*  Evaluates a jsish statement in the top-level environment. 
    Returns undefined in most cases to accommodate for return values that may result
    from the generic evaluation statements used for block, if, and while statements.  *)
and evalStatement state (ST_EXP {expr}) = (evalExpr state expr; UNDEFINED)
  | evalStatement state (BLOCK_ST {stmts}) =
    evalBlockStatement evalStatement state stmts
  | evalStatement state (IF_ST {guard, thenBlock, elseBlock}) =
    evalIfStatement evalStatement state guard thenBlock elseBlock
  | evalStatement state (PRINT_ST {expr}) = (evalPrintStatement state expr; UNDEFINED)
  | evalStatement state (WHILE_ST {guard, block}) =
    evalWhileStatement evalStatement state guard block
  (*| evalStatement state GC_ST = (collectGarbage state; UNDEFINED)*)
  (*| evalStatement state INUSE_ST = (checkInUse state; UNDEFINED)*)
  | evalStatement state (RETURN_ST {expr}) = raise Return

(*  Evaluates a jsish function declaration, creating a function value for a 
    closure and assigning it to an identifier. *)
and evalFunctionDec state (EXP_FUNCTION {id=EXP_ID id, params, body}) =
    bindValueInCurrentEnv state (first (createClosure state params body)) id
  | evalFunctionDec state expr = raise FunctionDeclaration ("temporarily broken -- no expression to string")

(*  Evaluates a jsish variable declaration, binding a value to an identifier in
    the current stateironment. *)
and evalVariableDec state (VAR_DEC {id=EXP_ID id}) =
    declareValueInCurrentEnv state id
  | evalVariableDec state (VAR_INIT {id=EXP_ID id, expr}) =
    bindValueInCurrentEnv state (evalExpr state expr) id
  | evalVariableDec state variable =
    raise VariableIdentifier ("temporarily broken -- no expression to string")

(*  Evaluates a jsish source element. *)
and evalSrcElem state (STMT {stmt}) = (evalStatement state stmt; ())
  | evalSrcElem state (FN_DEC expression) = evalFunctionDec state expression
  | evalSrcElem state (VAR_ELEM {vars}) = app (evalVariableDec state) vars

(*  Evaluates a jsish program. *)
fun evalProgram state (PROGRAM {elems}) = app (evalSrcElem state) elems

(*  Interpret the jsish program contained in the given filename. *)
fun interpret filename = 
    let
        val state = initState ()
    in
        evalProgram state (parse filename)
    end
    handle InvalidType msg => error msg
         | InvalidIdentifier id => error ("variable '" ^ id ^ "' not found")
         | VariableIdentifier id => error ("identifier '" ^ id ^ "' is not a valid variable name")
         | This => error ("'this' does not exist in this context")
         | Return => error "return statements are only valid inside functions"
         | DivideByZero => error "division by zero"
         | UnaryOperation (expected, found, oper) => error ("unary operator '" ^ oper ^ "' requires " ^ expected ^ ", found " ^ found)
         | BinaryOperation (expected, found, oper) => error ("operator '" ^ oper ^ "' requires " ^ expected ^ ", found " ^ found)
         | TernaryGuard found => error ("boolean guard required for 'cond' expression, found " ^ found)
         | IfGuard found => error ("boolean guard required for 'if' statement, found " ^ found)
         | WhileGuard found => error ("boolean guard required for 'while' statement, found " ^ found)
         | NewOperand found => error ("new may only be applied to a function, found " ^ found)
         | FunctionDeclaration expr => error ("invalid declaration of function expression " ^ sq expr)
         | FunctionInvocation value => error ("attempt to invoke '" ^ value ^ "' value as a function")
         | LeftHandSide expr => error ("invalid left-hand side expression " ^ sq expr)

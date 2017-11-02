use "printAST.sml";
use "value.sml";

exception NotFound
exception InvalidIdentifer of string

(*  HashTable functions *)
val setState = HashTable.insert
val stateFind = HashTable.find
val stateGet = HashTable.lookup

(*  Dies from a type error from a binary operation for the given type raised by
    the two given expressions. *)
fun binInvalidType state reqType lft rht oper =
    let
        val typeL = typeof (evalExpr state lft)
        val typeR = typeof (evalExpr state rht)
        val required = reqType ^ " * " ^ reqType
        val found = typeL ^ " * " ^ typeR
        val msg = "operator '" ^ oper ^ "' requires " ^ required ^ ", found " ^ found
    in
        raise InvalidType msg
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
    handle UnwrapMismatch (expected, found) => binInvalidType state expected lft rht oper

(*  Applies the given relational operation to the two number expressions.
    Takes the operator as a string for error messages. *)
and appRelOp state f oper lft rht =
    let
        val first = unwrapNum (evalExpr state lft)
        val second = unwrapNum (evalExpr state rht)
    in
        BOOL (f (first, second))
    end
    handle UnwrapMismatch (expected, found) => binInvalidType state expected lft rht oper

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
) handle UnwrapMismatch (expected, found) => binInvalidType state expected lft rht "&&"

(*  Evaluates a jsish logical OR operation for the given expressions. *)
and evalOr state lft rht = BOOL (
    unwrapBool (evalExpr state lft) orelse unwrapBool (evalExpr state rht)
) handle UnwrapMismatch (expected, found) => typeError expected found "||"

(*  Evaluates a jsish binary operation with the given operator and expressions. *)
and evalBinExpr state BOP_PLUS lft rht = evalOp state addVals lft rht
  | evalBinExpr state BOP_MINUS lft rht = appMathOp state (op -) "-" lft rht
  | evalBinExpr state BOP_TIMES lft rht = appMathOp state (op * ) "*" lft rht
  | evalBinExpr state BOP_DIVIDE lft rht = appMathOp state (op div) "/" lft rht
  | evalBinExpr state BOP_MOD lft rht = appMathOp state (op mod) "%" lft rht
  | evalBinExpr state BOP_EQ lft rht = evalOp state areValsEq lft rht
  | evalBinExpr state BOP_NE lft rht = evalOp state areValsNotEq lft rht
  | evalBinExpr state BOP_LT lft rht = appRelOp state (op <) "<" lft rht
  | evalBinExpr state BOP_GT lft rht = appRelOp state (op >) ">" lft rht
  | evalBinExpr state BOP_LE lft rht = appRelOp state (op <=) "<=" lft rht
  | evalBinExpr state BOP_GE lft rht = appRelOp state (op >=) ">=" lft rht
  | evalBinExpr state BOP_AND lft rht = evalAnd state lft rht
  | evalBinExpr state BOP_OR lft rht = evalOr state lft rht
  | evalBinExpr state BOP_COMMA lft rht = (evalExpr state lft; evalExpr state rht)

(*  Evaluates a jsish logical NOT operation on the given expression. *)
and evalNot state expr = BOOL (
    not (unwrapBool (evalExpr state expr))
) handle UnwrapMismatch (expected, found) => unaryInvalidType expected found "!"

(*  Evaluates a jsish unary MINUS operation on the given expression. *)
and evalNegative state expr = NUMBER (
    ~ (unwrapNum (evalExpr state expr))
) handle UnwrapMismatch (expected, found) => unaryInvalidType expected found "-"

(*  Evaluates a jsish unary operation with the given operator and expression. *)
and evalUnExpr state UOP_TYPEOF EXP_TRUE = STRING ("boolean")
  | evalUnExpr state UOP_TYPEOF EXP_FALSE = STRING ("boolean")
  | evalUnExpr state UOP_TYPEOF EXP_UNDEFINED = STRING ("undefined")
  | evalUnExpr state UOP_TYPEOF (EXP_CLOSURE _) = STRING ("function")
  | evalUnExpr state UOP_TYPEOF (EXP_NUM _) = STRING ("number")
  | evalUnExpr state UOP_TYPEOF (EXP_STRING _) = STRING ("string")
  | evalUnExpr state UOP_NOT expr = evalNot state expr
  | evalUnExpr state UOP_MINUS expr = evalNegative state expr

(*  Evaluates a jsish assignment, assigning the value of the rhs to the lhs id. *)
and evalAssignment state (EXP_ID id) rhs =
    let
        val rhsVal = evalExpr state rhs
    in
        (setState state (id, rhsVal); rhsVal)
    end
  | evalAssignment state other rhs =
    raise LeftHandSide ("invalid left-hand side expression '" ^ (exprString other) ^ "'")

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
  
(*  Evaluates an expression to a jsish value. *)
and evalExpr state EXP_TRUE = BOOL true
  | evalExpr state EXP_FALSE = BOOL false
  | evalExpr state EXP_UNDEFINED = UNDEFINED
  | evalExpr state (EXP_CLOSURE {id, params, body}) = UNDEFINED
  | evalExpr state (EXP_FN_CALL {id, args}) = UNDEFINED
  | evalExpr state (EXP_NUM num) = NUMBER num
  | evalExpr state (EXP_STRING st) = STRING st
  | evalExpr state (EXP_BINARY {opr, lft, rht}) = evalBinExpr state opr lft rht
  | evalExpr state (EXP_UNARY {opr, opnd}) = evalUnExpr state opr opnd
  | evalExpr state (EXP_ASSIGN {lhs, rhs}) = evalAssignment state lhs rhs
  | evalExpr state (EXP_COND {guard, thenExpr, elseExpr}) =
    evalCondExpr state guard thenExpr elseExpr
  | evalExpr state (EXP_ID id) = (stateGet state id)
    handle NotFound => raise InvalidIdentifer (id)

(*  Evaluates a jsish block statement. *)
and evalBlock state stmts = app (evalStatement state) stmts

(*  Evaluates an expression to a jsish value and prints the result. *)
and printExpr state expr = 
    let
        val eval = evalExpr state expr
        val evalStr = valToString eval
    in
        printOut evalStr
    end

(*  Evaluates a jsish while statement. *)
and evalWhileStatement state guard block =
    let
        val cond = evalExpr state guard
    in
        if unwrapBool cond
        then (
            evalStatement state block;
            evalWhileStatement state guard block)
        else ()
    end
    handle UnwrapMismatch (expected, found) => 
    raise InvalidType ("boolean guard required for 'while' statement, found " ^ found)

(*  Evaluates a jsish if statement. *)
and evalIfStatement state guard thenBlock elseBlock = (
    if unwrapBool (evalExpr state guard)
    then evalStatement state thenBlock
    else evalStatement state elseBlock; ())
    handle UnwrapMismatch (expected, found) => 
    raise InvalidType ("boolean guard required for 'if' statement, found " ^ found)

(*  Evaluates a jsish statement expression. *)
and evalStatementExpr state expr = (evalExpr state expr; ())

(*  Evaluates a jsish statement. *)
and evalStatement state (ST_EXP {expr}) = evalStatementExpr state expr
  | evalStatement state (BLOCK_ST {stmts}) = evalBlock state stmts
  | evalStatement state (PRINT_ST {expr}) = printExpr state expr
  | evalStatement state (WHILE_ST {guard, block}) = evalWhileStatement state guard block
  | evalStatement state (IF_ST {guard, thenBlock, elseBlock}) =
    evalIfStatement state guard thenBlock elseBlock

(*  Evaluates a jsish function. *)
and evalFunction state (EXP_CLOSURE {id=EXP_ID id, params, body}) =
    setState state (id, UNDEFINED)

(*  Evaluates a jsish source element. *)
fun evalSrcElem state (STMT {stmt}) = evalStatement state stmt
  | evalSrcElem state (FN_DEC expression) = evalFunction state expression
  | evalSrcElem state (VAR_ELEM {vars}) = ()

(*  Evaluates a jsish program. *)
fun evalProgram state (PROGRAM {elems}) = app (evalSrcElem state) elems

(*  Interpret the jsish program contained in the given filename. *)
fun interpret filename = 
    let
        (* Create a hash table to represent the state. *)
        val hash_fn = HashString.hashString
        val cmp_fn = (op =)
        val initial_size = 101
        val tbl = HashTable.mkTable (hash_fn, cmp_fn) (initial_size, NotFound)
    in
        evalProgram tbl (parse filename)
    end
    handle InvalidType msg => error msg
         | InvalidIdentifer id => error ("variable '" ^ id ^ "' not found")

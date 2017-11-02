use "parser.sml";

val concatWithComma = String.concatWith ", "

(* Converts a jsish binary operator to a string. *)
fun bopToString BOP_PLUS = "+"
  | bopToString BOP_MINUS = "-"
  | bopToString BOP_TIMES = "*"
  | bopToString BOP_DIVIDE = "/"
  | bopToString BOP_MOD = "%"
  | bopToString BOP_EQ = "=="
  | bopToString BOP_NE = "!="
  | bopToString BOP_LT = "<"
  | bopToString BOP_GT = ">"
  | bopToString BOP_LE = "<="
  | bopToString BOP_GE = ">="
  | bopToString BOP_AND = "&&"
  | bopToString BOP_OR = "||"
  | bopToString BOP_COMMA = ","

(* Converts a jsish unary operator to a string. *)
fun uopToString UOP_NOT = "!"
  | uopToString UOP_TYPEOF = "typeof"
  | uopToString UOP_MINUS = "-"

(* Converts a jsish binary expression to a string. *)
fun binaryExprString opr lft rht =
    let
        val lStr = exprString lft
        val opStr = bopToString opr
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
and assignmentExprString lhs rhs =
    let
        val lhsStr = exprString lhs
        val rhsStr = exprString rhs
    in
        paren (lhsStr ^ " = " ^ rhsStr)
    end

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
  | exprString (EXP_BINARY {opr, lft, rht}) =
    binaryExprString opr lft rht
  | exprString (EXP_UNARY {opr, opnd}) =
    unaryExprString opr opnd
  | exprString (EXP_COND {guard, thenExpr, elseExpr}) =
    conditionalExprString guard thenExpr elseExpr
  | exprString (EXP_ASSIGN {lhs, rhs}) =
    assignmentExprString lhs rhs
  | exprString (EXP_CLOSURE {id, params, body}) =
    functionExprString id params body
  | exprString (EXP_FN_CALL {id, args}) =
    functionCallString id args

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
  | statementString (RETURN_ST {value}) = returnStatementString value
  | statementString (IF_ST {guard, thenBlock, elseBlock}) =
    ifStatementString guard thenBlock elseBlock

(* Converts a jsish function declaration to a string. *)
and functionDecString (EXP_CLOSURE {id, params, body}) = 
    concat [
        line ("function " ^ (exprString id) ^ "(" ^ (paramListString params) ^ ")"),
        line "{",
        concat (map srcElemString body),
        line "}"
    ]

(* Converts a jsish variable declaration to a string. *)
and varDecString (VAR_DEC {id, value=EXP_UNDEFINED}) = exprString id
  | varDecString (VAR_DEC {id, value=EXP_ASSIGN {lhs, rhs}}) = exprString lhs ^ " = " ^ exprString rhs
  | varDecString (VAR_DEC {id, value=value}) = (exprString id) ^ " = " ^ (exprString value)

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

exception ConversionError of string

(* The tokens used in the jsish language. *)
datatype token = 
    TK_NUM of int
  | TK_ID of string
  | TK_STRING of string
  | TK_BOOL of bool
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
  (* Punctuation *)
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


(* Returns the string representation of a token. *)
fun tokenToString (TK_ID id) = id
  | tokenToString (TK_NUM num) = Int.toString num
  | tokenToString (TK_STRING st) = "string: " ^ st
  | tokenToString (TK_BOOL true) = "true"
  | tokenToString (TK_BOOL false) = "false"
  | tokenToString TK_ELSE = "else"
  | tokenToString TK_FUNCTION = "function"
  | tokenToString TK_IF = "if"
  | tokenToString TK_NEW = "keyword: new"
  | tokenToString TK_PRINT = "print"
  | tokenToString TK_RETURN = "return"
  | tokenToString TK_THIS = "keyword: this"
  | tokenToString TK_TYPEOF = "typeof"
  | tokenToString TK_UNDEFINED = "undefined"
  | tokenToString TK_VAR = "var"
  | tokenToString TK_WHILE = "while"
  | tokenToString TK_LBRACE = "{"
  | tokenToString TK_RBRACE = "}"
  | tokenToString TK_LPAREN = "("
  | tokenToString TK_RPAREN = ")"
  | tokenToString TK_LBRACKET = "["
  | tokenToString TK_RBRACKET = "]"
  | tokenToString TK_COMMA = ","
  | tokenToString TK_SEMI = ";"
  | tokenToString TK_QUESTION = "?"
  | tokenToString TK_COLON = ":"
  | tokenToString TK_DOT = "."
  | tokenToString TK_ASSIGN = "="
  | tokenToString TK_AND = "&&"
  | tokenToString TK_OR = "||"
  | tokenToString TK_NOT = "!" 
  | tokenToString TK_EQ = "==" 
  | tokenToString TK_NE = "!=" 
  | tokenToString TK_LT = "<" 
  | tokenToString TK_GT = ">" 
  | tokenToString TK_LE = "<=" 
  | tokenToString TK_GE = ">=" 
  | tokenToString TK_PLUS = "+" 
  | tokenToString TK_MINUS = "-" 
  | tokenToString TK_TIMES = "*" 
  | tokenToString TK_DIVIDE = "/" 
  | tokenToString TK_MOD = "%" 
  | tokenToString TK_EOF = "eof"

(* Checks if the given token is a unary operator. *)
fun isUnaryOp TK_NOT = true
  | isUnaryOp TK_TYPEOF = true
  | isUnaryOp TK_MINUS = true
  | isUnaryOp _ = false

(* Converts a string to a Keyword Token. *)
fun strToKeyword "else" = TK_ELSE
  | strToKeyword "false" = TK_BOOL false
  | strToKeyword "function" = TK_FUNCTION
  | strToKeyword "if" = TK_IF
  | strToKeyword "new" = TK_NEW
  | strToKeyword "print" = TK_PRINT 
  | strToKeyword "return" = TK_RETURN 
  | strToKeyword "this" = TK_THIS
  | strToKeyword "true" = TK_BOOL true
  | strToKeyword "typeof" = TK_TYPEOF 
  | strToKeyword "undefined" = TK_UNDEFINED 
  | strToKeyword "var" = TK_VAR
  | strToKeyword "while" = TK_WHILE
  | strToKeyword other = raise ConversionError (other ^ " is not a keyword")

(* Converts an Identifier Token to a Keyword Token. *)
fun idToKeyword (TK_ID value) = strToKeyword value

(* Checks if the given string is a keyword in jsish. *)
fun isStrKeyword value = let val keyword = strToKeyword value in true end
    handle ConversionError msg => false

(* Checks if the given token is a keyword in jsish. *)
fun isTknKeyword (TK_ID value) = isStrKeyword value | isTknKeyword _ = false

(* Converts a string to a Symbol Token. *) 
fun strToSymbol "{" = TK_LBRACE
  | strToSymbol "}" = TK_RBRACE
  | strToSymbol "(" = TK_LPAREN
  | strToSymbol ")" = TK_RPAREN
  | strToSymbol "[" = TK_LBRACKET
  | strToSymbol "]" = TK_RBRACKET
  | strToSymbol "," = TK_COMMA
  | strToSymbol ";" = TK_SEMI
  | strToSymbol "?" = TK_QUESTION
  | strToSymbol ":" = TK_COLON
  | strToSymbol "." = TK_DOT
  | strToSymbol "=" = TK_ASSIGN
  | strToSymbol "&&" = TK_AND
  | strToSymbol "||" = TK_OR
  | strToSymbol "!" = TK_NOT
  | strToSymbol "==" = TK_EQ
  | strToSymbol "!=" = TK_NE
  | strToSymbol "<" = TK_LT
  | strToSymbol ">" = TK_GT
  | strToSymbol "<=" = TK_LE
  | strToSymbol ">=" = TK_GE
  | strToSymbol "+" = TK_PLUS
  | strToSymbol "-" = TK_MINUS
  | strToSymbol "*" = TK_TIMES
  | strToSymbol "/" = TK_DIVIDE
  | strToSymbol "%" = TK_MOD
  | strToSymbol other = raise ConversionError ("invalid symbol: '" ^ other ^ "'")


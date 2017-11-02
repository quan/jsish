Control.Print.printDepth := 20;
Control.Print.printLength := 100;

exception ConversionError of string

(* Returns a string representation of a negative integer. *)
fun negativeIntToString num = "-" ^ Int.toString(~num)

(* Returns the string representation of an integer. *)
fun intToString num = (if num < 0 then negativeIntToString else Int.toString) num

(* Converts a boolean to a string. *)
fun boolToString true = "true"
  | boolToString false = "false"

(* Parenthesizes a string. *)
fun paren s = "(" ^ s ^ ")"

(* Indents a string. *)
fun indent s = "   " ^ s

(* Converts a string to a line. *)
fun line str = str ^ "\n"

(* Convenience functions for checking character values. *)
val isSpace = Char.isSpace
val isAlpha = Char.isAlpha
val isDigit = Char.isDigit
fun isNotDigit c = not (isDigit c)
fun isAlnum c = isAlpha c orelse isDigit c
fun isNotAlnum c = not (isAlnum c)
fun isQuotes c = Char.compare (c, #"\"") = EQUAL
fun isNotQuotes c = not (isQuotes c)

(* Checks if a given character is a backslash. *)
fun isEscChar #"\\" = true | isEscChar ch = false

(* Converts all unescaped escape chars in a string into escape chars. *)
fun escape s =
    let (* Converts all unescaped escape chars in a char list into escape chars. *)
        fun convert [] = []
          | convert (#"\\"::xs) = escapeCh xs
          | convert (ch::xs) = ch::convert xs
        and escapeCh (#"\\"::xs) = #"\\"::convert xs
          | escapeCh (#"\""::xs) = #"\""::convert xs
          | escapeCh (#"b"::xs) = #"\b"::convert xs
          | escapeCh (#"f"::xs) = #"\f"::convert xs
          | escapeCh (#"n"::xs) = #"\n"::convert xs
          | escapeCh (#"r"::xs) = #"\r"::convert xs
          | escapeCh (#"t"::xs) = #"\t"::convert xs
          | escapeCh (#"v"::xs) = #"\v"::convert xs
    in implode (convert (explode s)) end

(* Converts all escape chars in a string into characters. *)
fun unescape s =
    let (* Converts all escape chars in a char list into unescaped chars. *)
        fun convert [] = []
          | convert (#"\""::xs) = [#"\\", #"\""]@convert xs
          | convert (#"\b"::xs) = [#"\\", #"b"]@convert xs
          | convert (#"\f"::xs) = [#"\\", #"f"]@convert xs
          | convert (#"\n"::xs) = [#"\\", #"n"]@convert xs
          | convert (#"\r"::xs) = [#"\\", #"r"]@convert xs
          | convert (#"\t"::xs) = [#"\\", #"t"]@convert xs
          | convert (#"\v"::xs) = [#"\\", #"v"]@convert xs
          | convert (ch::xs) = ch::convert xs
    in implode (convert (explode s)) end

(* Prints a string to a outstream. *)
fun printStrTo out s = TextIO.output (out, s)

(* Prints a line to standard out. *)
fun printLn s = printStrTo TextIO.stdOut (s ^ "\n")

(* Prints a string to standard out. *)
val printOut = printStrTo TextIO.stdOut

(* Print an error and die. *)
fun error s = (
    printStrTo TextIO.stdErr (s ^ "\n");
    OS.Process.exit OS.Process.failure
)

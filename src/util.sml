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

exception ConversionError of string
exception InvalidEscape of string

fun first (a, _) = a
fun second (_, b) = b

(* Returns the string representation of a negative integer. *)
fun negativeIntToString num = "-" ^ Int.toString(~num)

(* Returns the string representation of an integer. *)
fun intToString num =
    if num < 0
    then negativeIntToString num
    else Int.toString num

fun boolToString true = "true"
  | boolToString false = "false"

fun paren s = "(" ^ s ^ ")"
fun indent s = "   " ^ s
fun line str = str ^ "\n"
(* Wraps a string in single quotes. *)
fun sq s = "'" ^ s ^ "'"
(* Wraps a character in single quotes. *)
val sqstr = sq o str

val concatWithComma = String.concatWith ", "

(* Matches the first item in a pair and returns its counterpart. *)
fun findMatch exp pairs = (
    case List.find (fn (v, _) => v = exp) pairs of
        NONE => NONE
      | SOME (_, v) => SOME v
)

(* Matches the second item in a pair and returns its counterpart. *)
fun findMatchRev exp pairs = (
    case List.find (fn (_, v) => v = exp) pairs of
        NONE => NONE
      | SOME (v, _) => SOME v
)

(* Convenience functions for checking character values. *)
val isSpace = Char.isSpace
val isAlpha = Char.isAlpha
val isDigit = Char.isDigit
fun isNotDigit c = not (isDigit c)
fun isAlnum c = isAlpha c orelse isDigit c
fun isNotAlnum c = not (isAlnum c)
fun isQuotes c = Char.compare (c, #"\"") = EQUAL
fun isNotQuotes c = not (isQuotes c)

val escapeChars = [#"\\",  #"\"", #"b", #"f", #"n", #"r", #"t", #"v"]
fun isEscChar c = List.exists (fn x => x = c) escapeChars

(* Converts all unescaped escape chars in a string into escape chars. *)
fun escape s =
    let (* Converts all unescaped escape chars in a char list into escape chars. *)
        fun convert [] = []
          | convert (#"\\"::cs) = escapeCh cs
          | convert (ch::cs) = ch::convert cs
        and escapeCh (#"\\"::cs) = #"\\"::convert cs
          | escapeCh (#"\""::cs) = #"\""::convert cs
          | escapeCh (#"b"::cs) = #"\b"::convert cs
          | escapeCh (#"f"::cs) = #"\f"::convert cs
          | escapeCh (#"n"::cs) = #"\n"::convert cs
          | escapeCh (#"r"::cs) = #"\r"::convert cs
          | escapeCh (#"t"::cs) = #"\t"::convert cs
          | escapeCh (#"v"::cs) = #"\v"::convert cs
          | escapeCh (c::cs) = raise InvalidEscape ("\\" ^ str c)
    in (implode o convert o explode) s end

(* Converts all escape chars in a string into characters. *)
fun unescape s =
    let (* Converts all escape chars in a char list into unescaped chars. *)
        fun convert [] = []
          | convert (#"\""::cs) = [#"\\", #"\""]@convert cs
          | convert (#"\b"::cs) = [#"\\", #"b"]@convert cs
          | convert (#"\f"::cs) = [#"\\", #"f"]@convert cs
          | convert (#"\n"::cs) = [#"\\", #"n"]@convert cs
          | convert (#"\r"::cs) = [#"\\", #"r"]@convert cs
          | convert (#"\t"::cs) = [#"\\", #"t"]@convert cs
          | convert (#"\v"::cs) = [#"\\", #"v"]@convert cs
          | convert (c::cs) = c::convert cs
    in (implode o convert o explode) s end

(* Prints a string to a outstream. *)
fun printStrTo out s = TextIO.output (out, s)
val printLn = printStrTo TextIO.stdOut o line
val printErr = printStrTo TextIO.stdErr o line
val printOut = printStrTo TextIO.stdOut

(* Prints an error and dies. *)
fun error s = (
    printErr s;
    OS.Process.exit OS.Process.failure
)

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
use "tokens.sml";

exception StringNotTerminated
exception UnexpectedChar of char
exception UnexpectedEOF

(*  Returns the next character value in the given filestream. *)
(*  Does not consume the character. *)
fun peekChar fstream = valOf (TextIO.lookahead fstream)

(*  Reads the next character value from the given filestream.  *)
(*  Consumes the character. *)
fun nextChar fstream = valOf (TextIO.input1 fstream)
    handle Option => raise UnexpectedEOF

(*  Checks if the given string is a symbol in jsish. *)
fun isSymbol str = let val symbol = stringToSymbol str in true end
    handle Match => false

(*  Checks if the combination of symbols consistutes a double symbol. *)
fun isSymbolPair "&" "&" = true
  | isSymbolPair "|" "|" = true
  | isSymbolPair "=" "=" = true
  | isSymbolPair "!" "=" = true
  | isSymbolPair "<" "=" = true
  | isSymbolPair ">" "=" = true
  | isSymbolPair _ _ = false

(*  Builds upon a token string with the next char read from a filestream. *)
fun buildToken token fstream = token ^ str (nextChar fstream)

(*  Reads a token from the filestream, one character at a time, where the token
    is defined by the given validator that determines which characters are allowed
    to follow the current token and the given delimiter that determines which
    characters are allowed the close the token and a special case to be handled
    by the given handler. Returns the token as a string. *)
fun readGenericToken specialCase handler isValid isDelimiter token fstream =
    let
        val nextCh = peekChar fstream
        val recurse = readGenericToken specialCase handler isValid isDelimiter
    in
        (* Handle a special case. *)
        if specialCase nextCh
        then handler (buildToken token fstream) fstream
        (* Continue reading if a valid next character is encountered. *)
        else if isValid nextCh
        then recurse (buildToken token fstream) fstream
        (* Return the token if a valid stop character is encountered. *)
        else if isDelimiter nextCh
        then token
        else raise UnexpectedChar nextCh
    end

(*  A version of readGenericToken with no special cases. *)
val readSimpleToken = readGenericToken (fn v => false) (fn t => fn f => "")

(*  Reads a Keyword/Identifier Token beginning with the given character string
    from the filestream. *)
fun readIdOrKeyword first fstream = 
    let
        val tkn = TK_ID (readSimpleToken isAlnum isNotAlnum first fstream)
    in
        if isKeyword tkn
        then idToKeyword tkn
        else tkn
    end

(* Reads a Number Token beginning with the given string from the filestream. *)
fun readNumber first fstream = TK_NUM (
    valOf (Int.fromString (readSimpleToken isDigit isNotDigit first fstream))
)

(* Builds upon a string with characters read from the given filestream with a 
   special case for escaped characters. *)
fun parseString tkn fstream =
    readGenericToken (fn c => c = #"\\") readEscChar isNotQuotes isQuotes tkn fstream

(* Handles escaped characters within strings. *)
and readEscChar tkn fstream =
    readGenericToken isEscChar parseString (fn _ => false) (fn _ => false) tkn fstream
    
(*  Reads a String token from the filestream. *)
fun readString fstream = TK_STRING (
    let 
        val token = escape (parseString "" fstream)
    in  (* Read and discard the closing quotes. *)
        (nextChar fstream; token)
    end
) handle Option => raise StringNotTerminated

(*  Reads the given symbol token from filestream, checking for symbols 
    consisting of two characters. *)
fun readSymbol tkn fstream = 
    let
        val first = tkn
        val specialCase = (fn next => isSymbolPair first (str next))
        val handler = (fn token => fn fstream => token)
        (* Check for symbols consisting of two characters. *)
        val token = readGenericToken specialCase handler (fn _ => false) (fn _ => true) tkn fstream
    in
        stringToSymbol token
    end 


(* Reads the first character of a token to determine the type of token to read
   and passes control to the relevant handler. *)
fun recognizeToken fstream = 
    let 
        val nextCh = nextChar fstream
    in
        (* Skip spaces. *)
        if isSpace nextCh
        then ifNotEOF recognizeToken fstream
        (* Read an identifier/keyword if a letter is encountered. *)
        else if isAlpha nextCh
        then readIdOrKeyword (str nextCh) fstream
        (* Read a number if a digit is encountered. *)
        else if isDigit nextCh
        then readNumber (str nextCh) fstream
        (* Read a string if a quote is encountered, dropping the opening quote. *)
        else if isQuotes nextCh
        then readString fstream
        (* Otherwise read a symbol. *)
        else readSymbol (str nextCh) fstream
    end

(* Applies the given function to the filestream unless the end has been reached. *)
and ifNotEOF f fstream =
    if TextIO.endOfStream fstream
    then TK_EOF
    else f fstream

(* Returns the next token from a provided file stream. *)
val nextToken = ifNotEOF recognizeToken
    handle InvalidSymbol symbol => error ("invalid symbol: '" ^ symbol ^ "'")
         | InvalidEscape seq => error ("invalid escape sequence: " ^ sq seq) 
         | NotKeyword token => error (token ^ " is not a keyword")
         | StringNotTerminated => error "string not terminated"
         | UnexpectedChar c => error ("encountered unexpected char '" ^ str c ^ "'")
         | UnexpectedEOF => error ("unexpected eof during tokenization")

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

use "value.sml";
use "table.sml";

exception InvalidAddress of int
exception InvalidIdentifier of string

(* Extract state values. *)
fun stateEnvs {envs, heap, calls, pending} = envs
fun stateHeap {envs, heap, calls, pending} = heap
fun callStack {envs, heap, calls, pending} = calls
fun pendingObjs {envs, heap, calls, pending} = pending
fun heapTable {envs, heap={nextAddr, table}, calls, pending} = table
fun heapAddr {envs, heap={nextAddr, table}, calls, pending} = nextAddr
fun currentEnv state = (hd o stateEnvs) state

(* Environment aliases for hash table functions. *)
val defined = contains
val bind = put

fun createEnv () = createTable HashString.hashString
fun createHeap () = createTable Word.fromInt

(* Finds the most local environment in which an identifier has been defined. *)
fun findDefiningEnv [] id = raise NotFound
  | findDefiningEnv (env::envs) id = 
    if defined env id
    then env
    else findDefiningEnv envs id

(* Looks up the value of an identifier. *)
fun lookup envs id = get (findDefiningEnv envs id) id

(* Binds a value to an identifier. *)
fun assign state value id = (bind (findDefiningEnv (stateEnvs state) id) id value)
    handle NotFound => raise InvalidIdentifier id
fun bindValueInCurrentEnv state value id = bind (currentEnv state) id value
fun definedInCurrentEnv state id = defined (currentEnv state) id

(* Declares a variable, setting it to undefined if it has not already been defined. *)
fun declareValueInCurrentEnv state id =
    if not (definedInCurrentEnv state id)
    then bindValueInCurrentEnv state UNDEFINED id
    else ()

(* Initializes the state, returning the state and the global object. *)
fun initState () = 
    let
        (* Create the top-level environment chain and heap. *)
        val tle = createEnv ()
        val envs = [tle]
        val heap = {nextAddr=ref 0, table=createHeap ()}
    in  
        ({envs=envs, heap=heap, calls=[], pending=[]}, tle)
    end

(* Creates and appends a new hash table to a list of environments. *)
fun newEnv envs = createEnv ()::envs

(* Creates a new state with an added environment scope pushed onto the call stack. *)
fun pushEnv state scope = 
    let
        val envs = newEnv scope
        val heap = stateHeap state
        val calls = hd envs::callStack state
        val pending = pendingObjs state
    in
        {envs=envs, heap=heap, calls=calls, pending=pending}
    end

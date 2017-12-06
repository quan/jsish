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

exception NotFound

(* Checks if an identifier is defined in an environment. *)
fun definedInEnv env id = isSome (HashTable.find env id)

(* Checks if an identifier is defined in the current environment. *)
fun definedInCurrentEnv envs id = definedInEnv (hd envs) id

(* Finds the most local environment in which an identifier has been defined. *)
fun findDefiningEnv [] id = raise NotFound
  | findDefiningEnv (env::envs) id = 
    if definedInEnv env id
    then env
    else findDefiningEnv envs id

(* Looks up the value of an identifier. *)
fun lookupValue envs id = valOf (HashTable.find (findDefiningEnv envs id) id)

(* Binds a value to an identifier in the given environment. *)
fun bindValueInEnv env value id = (HashTable.insert env (id, value); value)

(* Binds a value to an identifier in the current environment. *)
fun bindValueInCurrentEnv envs value id = bindValueInEnv (hd envs) value id

(* Binds a value to an identifier, adding the identifier to the current environment if it is not found. *)
fun bindValue envs value id = 
    let
        val definingEnv = (findDefiningEnv envs id) 
        (* If the identifier has not been used previously, add it to the global scope. *)
        handle NotFound => List.last envs
    in
        bindValueInEnv definingEnv value id
    end

(* Creates and appends a new hash table to a list of environments or returns a new list. *)
fun newEnv [] = 
    let
        val hash_fn = HashString.hashString
        val cmp_fn = (op =)
        val initial_size = 20
    in
        [HashTable.mkTable (hash_fn, cmp_fn) (initial_size, NotFound)]
    end
  | newEnv envs = (newEnv [])@envs
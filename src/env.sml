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

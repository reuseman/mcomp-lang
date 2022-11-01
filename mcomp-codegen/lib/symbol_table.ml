exception DuplicateEntry of Ast.identifier
exception NotFound of Ast.identifier

(** The symbol table that is composed by hashtables chained together.
  The head represent the innermost scope, going down the list represents
  going outwards. *)
type 'a t = SymbolTable of (Ast.identifier, 'a) Hashtbl.t list

(** Create a global empty scope.
  @return a new empty scope. 
*)
let empty_table () = SymbolTable [ Hashtbl.create 0 ]

(** Given a scope, create a new inner empty scope.
  @param table the scope to create a new inner scope for.
  @return a new empty scope.
*)
let begin_block = function
  | SymbolTable tables -> SymbolTable (Hashtbl.create 0 :: tables)

(** Given a scope, remove the innermost scope.
  @param table the scope to remove the innermost scope from.
  @return the new scope.
  @raise Failure if the scope is empty.
*)
let end_block = function
  | SymbolTable [] -> raise (Failure "Cannot end block of empty symbol table")
  | SymbolTable (_ :: tables) -> SymbolTable tables

(** Given a scope, a symbol and info about the symbol, add the symbol to the scope.
  @param table the scope to add the symbol to.
  @param symbol the symbol to add to the scope.
  @param info the info about the symbol to add to the scope.
  @return the new scope.
  @raise Failure if the scope is empty.
  @raise DuplicateEntry if the symbol is already in the scope.
*)

let add_entry symbol info = function
  | SymbolTable [] -> raise (Failure "Cannot add entry to empty symbol table")
  | SymbolTable (table :: xs) ->
      if Hashtbl.mem table symbol then raise (DuplicateEntry symbol)
      else Hashtbl.add table symbol info;
      SymbolTable (table :: xs)

(** Given a scope and a symbol, lookup the info about the symbol by
 searching from the innermost scope to the outermost scope.
  @param table the scope to find the symbol in.
  @param symbol the symbol to find in the scope.
  @return the info about the symbol.
  @raise NotFound if the symbol is not in the scope.
*)
let rec lookup symbol = function
  | SymbolTable [] -> raise (NotFound symbol)
  | SymbolTable (table :: xs) -> (
      try Hashtbl.find table symbol
      with Not_found -> lookup symbol (SymbolTable xs))

(** Given a list of (symbol, info) pairs, create a new scope with them.
  @param entries the list of (symbol, info) pairs to add to the scope.
  @return the new scope.
  @raise Failure if the scope is empty.
  @raise DuplicateEntry if any of the symbols are already in the scope.
*)
let of_alist list =
  let table = empty_table () in
  List.iter
    (fun (symbol, info) ->
      let _ = add_entry symbol info table in
      ())
    list;
  table

(** Given a scope and a function, iterate over the symbols in the scope.
  @param table the scope to iterate over.
  @param f the function to apply to each symbol.
  @raise Failure if the scope is empty.
*)
let iter f = function
  | SymbolTable [] -> raise (Failure "Cannot iterate over empty symbol table")
  | SymbolTable (table :: _) -> Hashtbl.iter f table

(** Given a scope, a function and an initial value, fold over the symbols in the scope.
  @param table the scope to fold over.
  @param f the function to apply to each symbol.
  @param init the initial value to fold over.
  @return the final value.
  @raise Failure if the scope is empty.
*)
let fold f init = function
  | SymbolTable [] -> raise (Failure "Cannot fold over empty symbol table")
  | SymbolTable (table :: _) -> Hashtbl.fold f table init

let show = function
  | SymbolTable [] -> raise (Failure "Cannot show empty symbol table")
  | SymbolTable (table :: xs) ->
      let show_table table =
        let keys = Hashtbl.fold (fun key _ acc -> key :: acc) table [] in
        "[" ^ String.concat ", " keys ^ "]"
      in
      let tables = table :: xs in
      String.concat " | " (List.map show_table tables)

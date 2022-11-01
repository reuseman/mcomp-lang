exception DuplicateEntry of Ast.identifier
exception NotFound of Ast.identifier

type 'a t = SymbolTable of (Ast.identifier, 'a) Hashtbl.t list

let empty_table () = SymbolTable( [Hashtbl.create 0] )

let begin_block = function
| SymbolTable(tables) -> SymbolTable(Hashtbl.create 0 :: tables)

let end_block = function
| SymbolTable([]) -> raise (Failure "Cannot end block of empty symbol table")
| SymbolTable(_ :: tables) -> SymbolTable(tables)


let add_entry symbol info = function
| SymbolTable([]) -> raise (Failure "Cannot add entry to empty symbol table")
| SymbolTable(table :: xs) -> 
  if Hashtbl.mem table symbol then raise (DuplicateEntry symbol)
  else Hashtbl.add table symbol info;
  SymbolTable(table :: xs)
  
let rec lookup symbol = function
| SymbolTable([]) -> raise (NotFound symbol)
| SymbolTable(table :: xs) -> 
  try Hashtbl.find table symbol
  with Not_found -> lookup symbol (SymbolTable(xs))


let of_alist list = 
  let table = empty_table () in
  List.iter (fun (symbol, info) -> let _ = add_entry symbol info table in ()) list;
  table

let iter f = function
| SymbolTable([]) -> raise (Failure "Cannot iterate over empty symbol table")
| SymbolTable(table :: _) -> Hashtbl.iter f table

let fold f init = function
| SymbolTable([]) -> raise (Failure "Cannot fold over empty symbol table")
| SymbolTable(table :: _) -> Hashtbl.fold f table init

let show = function
| SymbolTable([]) -> raise (Failure "Cannot show empty symbol table")
| SymbolTable(table :: xs) -> 
  let show_table table = 
    let keys = Hashtbl.fold (fun key _ acc -> key :: acc) table [] in
    "[" ^ String.concat ", " keys ^ "]"
  in
  let tables = table :: xs in
  String.concat " | " (List.map show_table tables)
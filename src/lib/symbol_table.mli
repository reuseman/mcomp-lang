exception DuplicateEntry of Ast.identifier
exception NotFound of Ast.identifier

type 'a t

val empty_table : unit -> 'a t
val begin_block : 'a t -> 'a t
val end_block : 'a t -> 'a t
val add_entry : Ast.identifier -> 'a -> 'a t -> 'a t
val lookup : Ast.identifier -> 'a t -> 'a
val of_alist : (Ast.identifier * 'a) list -> 'a t
val iter : (string -> 'a -> unit) -> 'a t -> unit
val fold : (string -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
val show : 'a t -> string

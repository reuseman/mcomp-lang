type binop =
  (* Math operands *)
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  (* Compare operands *)
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  (* Bool operands *)
  | And
  | Or
[@@deriving show, ord, eq]

type uop = Neg | Not [@@deriving show, ord, eq]
type identifier = string [@@deriving show, ord, eq]

(* Increment/Decrement operators with prefix postfix behavior *)
type incdec_typ = Inc | Dec [@@deriving show, ord, eq]
type incdec_order = Pre | Post [@@deriving show, ord, eq]

type typ =
  | TInt                        (* Type int *)
  | TFloat                      (* Type float *)
  | TBool                       (* Type bool *)
  | TChar                       (* Type char *)
  | TArray of typ * int option  (* Array type *)
  | TRef of typ                 (* Reference type *)
  | TVoid                       (* Type unit *)
  | TFun of typ list * typ      (* Type functions [paramerts] -> return_type *)
  | TInterface of identifier    (* Type of an interface *)
  | TComponent of identifier    (* Type of a component *)
[@@deriving show, ord, eq]

type ('a, 'b) annotated_node = { node : 'a; annot : 'b }
[@@deriving show, ord, eq]

type vdecl = identifier * typ [@@deriving show, ord, eq]

type 'a expr = ('a expr_node, 'a) annotated_node

and 'a expr_node =
  | LV of 'a lvalue                                       (* x or a[e] *)
  | Assign of 'a lvalue * 'a expr                         (* x=e or a[e]=e *)
  | AssignBinOp of 'a lvalue * binop * 'a expr            (* x+=e or a[e]+=e *)
  | ILiteral of int                                       (* Integer literal *)
  | FLiteral of float                                     (* Float literal *)
  | CLiteral of char                                      (* Char literal *)
  | BLiteral of bool                                      (* Bool literal *)
  | UnaryOp of uop * 'a expr                              (* Unary primitive operator *)
  | Address of 'a lvalue                                  (* Address of a variable *)
  | BinaryOp of binop * 'a expr * 'a expr                 (* Binary primitive operator *)
  | Call of identifier option * identifier * 'a expr list (* Function call f(...) *)
  | IncDec of 'a lvalue * incdec_typ * incdec_order       (* Increment/decrement and pre/post e.g. i++ *)
[@@deriving show, ord, eq]

and 'a lvalue = ('a lvalue_node, 'a) annotated_node

and 'a lvalue_node =
  | AccVar of identifier option * identifier  (* Variable access x, where the first identifier is an optional qualifier *)
  | AccIndex of 'a lvalue * 'a expr           (* Array indexing a[e] *)
[@@deriving show, ord, eq]

and 'a stmt = ('a stmt_node, 'a) annotated_node

and 'a stmt_node =
  | If of 'a expr * 'a stmt * 'a stmt                                 (* Conditional *)
  | While of 'a expr * 'a stmt                                        (* While loop *)
  | DoWhile of 'a expr * 'a stmt                                      (* Do-While loop *)
  | For of 'a expr option * 'a expr option * 'a expr option * 'a stmt (* For loop *)
  | Expr of 'a expr                                                   (* Expression statement e; *)
  | Return of 'a expr option                                          (* Return statement *)
  | Block of 'a stmtordec list                                        (* Block: grouping and scope *)
  | Skip
[@@deriving show, ord, eq]

and 'a stmtordec = ('a stmtordec_node, 'a) annotated_node

and 'a stmtordec_node =
  | LocalDecl of vdecl * 'a expr option  (* Local variable declaration with optional initializer *)
  | Stmt of 'a stmt     (* A statement *)
[@@deriving show, ord, eq]

and 'a fun_decl = {
  rtype : typ;
  fname : identifier;
  formals : vdecl list;
  body : 'a stmt option; (* None when is a declaration, Some when definition *)
}
[@@deriving show, ord, eq]

and 'a member_decl = ('a member_decl_node, 'a) annotated_node
[@@deriving show, ord, eq]

and 'a member_decl_node =
  (* A member of an interface or of a component *)
  | FunDecl of 'a fun_decl
  | VarDecl of vdecl * 'a expr option
[@@deriving show, ord, eq]

and 'a interface_decl = ('a interface_decl_node, 'a) annotated_node

and 'a interface_decl_node =
  | InterfaceDecl of {
      (* Interface declaration *)
      iname : identifier;
      declarations : 'a member_decl list;
    }
[@@deriving show, ord, eq]

and 'a component_decl = ('a component_decl_node, 'a) annotated_node

and 'a component_decl_node =
  | ComponentDecl of {
      (* Component declaration *)
      cname : identifier;
      uses : identifier list;
      provides : identifier list;
      definitions : 'a member_decl list;
    }
[@@deriving show, ord, eq]

and connection = Link of identifier * identifier * identifier * identifier
[@@deriving show, ord, eq]

and 'a compilation_unit =
  | CompilationUnit of {
      interfaces : 'a interface_decl list;
      components : 'a component_decl list;
      connections : connection list;
    }
[@@deriving show, ord, eq]

type located_compilation_unit = Location.code_pos compilation_unit
[@@deriving show, ord, eq]

type typed_compilation_unit = typ compilation_unit
[@@deriving show, ord, eq]


(* Extra type needed to keep track of all the top level declarations for Menhir *)
type 'a top_decl =
  | Interface of 'a interface_decl
  | Component of 'a component_decl
  | Connection of connection list
[@@deriving show, ord, eq]

(* --- Helper functions --- *)

(**
  Annotate a node with a given annotation.
  @param a The node to annotate.
  @param b The annotation to use.
  @return The annotated node.
*)
let annotate_node a b = { node = a; annot = b }

(**
  Check weather a type is a primitive type (e.g. int, float, bool, char)
  @return True if the type is a primitive type, false otherwise.
*)
let is_primitive = function 
| TInt | TFloat | TBool | TChar -> true
| _ -> false

(**
  Check weather a type is a scalar type (e.g. array or array reference)
  @return True if the type is a scalar type, false otherwise.
*)
let rec is_scalar = function
  | TArray _ -> true
  | TRef typ -> is_scalar typ
  | _ -> false

(**
  Check weather a binary operation is a math operation (e.g. +, -, *, /) 
  @return True if the binary operation is a math operation, false otherwise.
*)
let is_math_op = function
| Add | Sub | Mult | Div | Mod -> true
| _ -> false

(**
  Check weather a binary operation is a comparison operation (e.g. <, >, <=, >=)
  @return True if the binary operation is a comparison operation, false otherwise.
*)
let is_comp_op = function
  | Equal | Neq | Less | Leq | Greater | Geq -> true
  | _ -> false

(**
  Check weather a binary operation is a logical operation (e.g. &&, ||)
  @return True if the binary operation is a logical operation, false otherwise.
*)
let is_bool_op = function
| And | Or -> true
| _ -> false


(**
  Check weather a given typ is equal to a specific type or to the reference over the type.
  @param oracle_typ The type that will be compared to x.
  @param to_check The type to check.
  @return True if the types are equal (e.g. int == int, ...) or if the types are over a reference (e.g. int == ref(int), ...), false otherwise.
*)
let is_of_typ_or_reftyp oracle_typ to_check =
  match to_check with
  | TRef ttyp -> equal_typ oracle_typ ttyp
  | _ -> equal_typ oracle_typ to_check

(**
  Redefine the derived show over the type in order to have better human readable output.
  @return A string representation of the type.
*)
let rec show_typ = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TChar -> "char"
  | TArray (t, None) -> Printf.sprintf "%s[]" (show_typ t)
  | TArray (t, Some n) -> Printf.sprintf "%s[%d]" (show_typ t) n
  | TRef t -> Printf.sprintf "ref %s" (show_typ t)
  | TVoid -> "void"
  | TFun (args, ret) -> Printf.sprintf "(%s) -> %s" (String.concat ", " (List.map show_typ args)) (show_typ ret)
  | TInterface i -> Printf.sprintf "interface %s" i
  | TComponent c -> Printf.sprintf "component %s" c


(** This module is the implementation of the semantic checker, that
checks the semantic and typing rules. It works on an AST annotated
with locations and returns a new AST annotated with types. *)

exception Semantic_error of Location.code_pos * string
open Ast

(** A flag to enable the printing of the main symbol tables *)
let debug_mode = false
(** Infix operator to annotate node with its type *)
let (++) a b = annotate_node a b

(** Information added to the identifier, to keep track of the type and location in code *)
type identifier_info = Ast.identifier * Ast.typ * Location.code_pos

(** The symbol that is added as the value inside the symbol table to keep track of the namespace *)
type symbol =
  | VarSymbol       of identifier_info
  (* Function  with identifier and function_table*)
  | FunctionSymbol  of identifier_info * symbol Symbol_table.t
  (* Interface with identifier and declarations_table *)
  | InterfaceSymbol of identifier_info * symbol Symbol_table.t 
  (* Component with identifier and uses_table, provided_table, definitions_table *)
  | ComponentSymbol of identifier_info * symbol Symbol_table.t * symbol Symbol_table.t * symbol Symbol_table.t

(** The main symbol table that is passed around, one for the components and one for the interfaces. *)
type global_symbol_table = {
  components  : symbol Symbol_table.t;  (* Components table *)
  interfaces  : symbol Symbol_table.t;  (* Interfaces table *)
}

(** A symbol table to keep track of the current environment that it's being checked *)
type 'a env = {
  current_table : symbol Symbol_table.t;
  component : 'a Ast.component_decl_node;
  component_symbol : symbol;
}

let ignore_case number message = failwith (Printf.sprintf "Error %d. This should not happen. " number ^ message)
let ignore number = ignore_case number ""

(** Helpers to raise Semantic errors with an error message *)
let build_msg (err_msg: string) (help_msg: string) = 
  err_msg ^ "\n    Help: " ^ help_msg
let semantic_error loc err_msg help_msg =
  raise (Semantic_error (loc, build_msg err_msg help_msg))



(* STANDARD-LIBRARY SUB-MODULE*)

(**
  The Standard Library is composed by two interfaces: "App" and "Prelude". 
  The former is used to define the main function of the program and can be defined only once.
  The latter is used to provide the basic functions of the language (e.g. print and get) and
  it's implicitly used by every component.
*)
module StandardLibrary = struct
  (**
    Given a name and a signature, it returns the corresponding interface declaration.
    @param: the name of the interface
    @param: the signature of the interface
    @return: the interface declaration
  *)
  let build_interface name signatures = 
    let build_decl signature = 
      let (name, typ) = signature in
      match typ with 
      | Ast.TFun(s_formals, s_rtype) ->
        let formals = List.map (fun (typ) -> ("fake_identifier", typ)) s_formals in
        let node = Ast.FunDecl{ rtype=s_rtype; fname = name; formals; body=None} in
        Ast.annotate_node node Location.dummy_code_pos
      | _ -> ignore_case 1 "The standard library only provides functions signatures"
    in
    let decls = List.map build_decl signatures in
    let interface_node = Ast.InterfaceDecl{ iname = name; declarations = decls } in
    Ast.annotate_node interface_node Location.dummy_code_pos

    (**
      It builds the interfaces decl of the standard library by looking at the signatures defined in Mcomp_stdlib.
      @return a list of interfaces decl
    *)
    let build_interfaces () : 'a interface_decl list =
      let app_interface = build_interface "App" Mcomp_stdlib.app_signature in
      let prelude_interface = build_interface "Prelude" Mcomp_stdlib.prelude_signature in
      [app_interface; prelude_interface]

end
(* END STANDARD-LIBRARY SUB-MODULE*)



(* ================================== 1st VISIT - SEMANTIC RULES ==================================*)


(* INTERFACE-VISITOR SUB-MODULE*)

(**
  The InterfaceVisitor module is a visitor that checks the semantic rules for the interfaces. 
  By visiting them, it's going to track variables and functions declarations in the given table.
*)
module InterfaceVisitor = struct
  (**
    Check that the returned type of the function is a valid one.
    @param rtype The returned type of the function
    @param f_name The name of the function
    @param loc The location of the function
    @raise Semantic_error if the returned type is not valid.
  *)
  let is_function_return_type_legal rtype f_name loc = 
    match rtype with
    | Ast.TInt | Ast.TBool | Ast.TChar | Ast.TVoid -> ()
    | _ -> 
      let err_msg = Printf.sprintf "Function %s has an invalid return type '%s'." f_name (Ast.show_typ rtype) in
      let help_msg = Printf.sprintf "Change the return type to a valid one (e.g. int, bool, char, void)." in
        semantic_error loc err_msg help_msg

  (**
    Check that the variable type is a valid one.
    @param identifier The identifier of the variable
    @param typ The type of the variable
    @param loc The location of the variable
    @raise Semantic_error if the variable type is not valid.
  *)
  let is_variable_decl_legal identifier typ loc = 
    match typ with 
    (* Forbid variables with void type ->                   var my_var: void *)
    | Ast.TVoid -> 
      let err_msg = Printf.sprintf "Variable '%s' cannot have type 'void'." identifier in
      let help_msg = "'void' can only be used as a return type of a function." in
      semantic_error loc err_msg help_msg
    (* Forbid arrays with void type ->                      var my_var: void[]  *)
    | Ast.TArray(Ast.TVoid, _) ->
      let err_msg = Printf.sprintf "Variable '%s' cannot have type 'void[]'." identifier in
      let help_msg = "'void' can only be used as a return type of a function." in
      semantic_error loc err_msg help_msg
    (* Forbid multi-dimensional arrays with any type ->     var my_var: int[][]  *)
    | Ast.TArray(Ast.TArray(_), _) ->
      let err_msg = Printf.sprintf "Variable '%s' cannot be a multi-dimensional array." identifier in
      let help_msg = "Multi-dimensional arrays are not supported. Use a one dimensional array instead." in
      semantic_error loc err_msg help_msg
    (* Forbid arrays with a non-positive size ->            var my_var: int[0] *)
    | Ast.TArray(_, Some(n)) when n < 1 -> 
      let err_msg = Printf.sprintf "Variable '%s' cannot have a non-positive size." identifier in
      let help_msg = "Size of an array must be a positive integer." in
      semantic_error loc err_msg help_msg
      (* Forbid array references ->                         var my_var: int[] *)
    | Ast.TArray(typ, None) ->
      let err_msg = Printf.sprintf "Variable %s cannot be a reference to an array." identifier in
      let help_msg = Printf.sprintf "Add a 'int' size to the array definition (e.g. var %s : %s[13])." identifier (Ast.show_typ typ) in
      semantic_error loc err_msg help_msg
    (* Forbid reference variable to a non-primitive type -> var my_var: &void *)
    | Ast.TRef(typ) when not(Ast.is_primitive typ) ->
      let err_msg = Printf.sprintf "Variable '%s' cannot be a reference to a non-primitive type." identifier in
      let help_msg = "References can only be used with primitive types (i.e. int, bool, char)." in
      semantic_error loc err_msg help_msg
    | _ -> ()

  (**
    Visit a given interface declaration and check the semantic rules for it.
    @param i_name The name of the interface
    @param declarations_table The symbol table to keep track of the declarations
    @param declaration The member declaration to visit
    @return The symbol table with the new declarations
    @raise Semantic_error if the member (variable or function) is defined multiple times
  *)
  let visit_declaration i_name declarations_table declaration =
    let loc = declaration.annot in
    match declaration.node with
    | Ast.FunDecl { rtype; fname; formals; _ } ->
      begin
        is_function_return_type_legal rtype fname loc;
        let info = (fname, Ast.TFun (List.map snd formals, rtype), loc) in
        let value = FunctionSymbol(info, Symbol_table.empty_table ()) in
        try
          Symbol_table.add_entry fname value declarations_table
        with Symbol_table.DuplicateEntry(id) ->
          let err_msg = Printf.sprintf "The interface '%s' declared multiple times the function '%s'." i_name id in
          let help_msg = "Functions must have unique names. Rename it or remove it." in
          semantic_error loc err_msg help_msg
      end
    | Ast.VarDecl (identifier, typ) ->
      begin
        is_variable_decl_legal identifier typ loc;
        let info = (identifier, typ, loc) in
        let value = VarSymbol info in
        try
          Symbol_table.add_entry identifier value declarations_table
        with Symbol_table.DuplicateEntry(id) ->
          let err_msg = Printf.sprintf "The interface '%s' declared multiple times the variable '%s'." i_name id in
          let help_msg = "Variables must have unique names. Rename it or remove it." in
          semantic_error loc err_msg help_msg
      end

  (**
    Given an interface table and an interface declaration, it visit the interface declaration,
    and adds to the table a new entry with the interface name and the interface symbol.
    @param interface_table The table to add the interface to.
    @param interface_decl The interface declaration to visit.
    @return The interface table with the new entry.
    @raise Semantic_error if the interface is defined more than once.
  *)
  let visit_interface interfaces_table i_decl = 
    let loc = i_decl.annot in
    match i_decl.node with Ast.InterfaceDecl{ iname; declarations = declarations } ->
      let info = (iname, Ast.TInterface(iname), loc) in
      let declarations_table = List.fold_left (visit_declaration iname) (Symbol_table.empty_table ()) declarations in
      let value = InterfaceSymbol(info, declarations_table) in

      (* DEBUG *)
      if debug_mode then
        let declarations_out = Symbol_table.show declarations_table in
        Printf.printf "\nINTERFACE: %s\n" iname; 
        Printf.printf " Declarations table: %s\n" declarations_out;
      else ();
      (* END-DEBUG *)

      try 
        Symbol_table.add_entry iname value interfaces_table
      with Symbol_table.DuplicateEntry(id) ->
        let err_msg = Printf.sprintf "The interface '%s' is declared more than once." id in
        let help_msg = "Interfaces must have unique names. Rename it." in
        semantic_error loc err_msg help_msg

  (**
    The main function of the InterfaceVisitor module. It visits the interfaces declarations
    and returns the updated interface table.
    @param interfaces The interfaces declarations to visit.
    @param interfaces_table The table to add the interfaces to.
    @return The updated interfaces table.
  *)
  let visit_interfaces interfaces_table (interfaces: Location.code_pos interface_decl list)  = 
    List.fold_left visit_interface interfaces_table interfaces
end
(* END INTERFACE-VISITOR SUB-MODULE*)


(* COMPONENT VISITOR SUB-MODULE*)

(**
  The ComponentVisitor module is a visitor that checks the semantic rules for the components. 
  By visiting them, it's going to keep track variables and functions definitions in the given table.
*)
module ComponentVisitor = struct


  (**
    Check that a component defines a variable or a function that matches with
    a declaration of one of the provided interface.
    @param c_name The name of the component.
    @param i_name The name of the interface.
    @param definitions_table The table of the component definitions.
    @param decl_name The name of the declaration.
    @param decl_symbol The symbol of the declaration.
    @raise SemanticError if the component does not define a variable/function that matches the declaration.
  *)
  let check_component_implements_provided_declaration c_name i_name definitions_table decl_name decl_symbol =
    match decl_symbol with 
    (* Check that the component defines the Variable that has been declared in the interface and that the type matches *)
    | VarSymbol(_, decl_type, decl_loc) -> 
      begin
        try 
          let def_symbol = Symbol_table.lookup decl_name definitions_table in
          begin
            match def_symbol with
            | VarSymbol((_, def_type, def_loc)) -> 
              if decl_type = def_type then () else 
                let err_msg = Printf.sprintf "Component '%s' provides the interface '%s', but the defined variable '%s' has a different type from the declared one." c_name i_name decl_name in
                let help_msg = Printf.sprintf "Change the type of '%s' from '%s' to '%s'." decl_name (Ast.show_typ def_type) (Ast.show_typ decl_type) in
                semantic_error def_loc err_msg help_msg
            | FunctionSymbol((_, _, def_loc),_) -> 
              let err_msg = Printf.sprintf "Component '%s' provides the interface '%s', but the defined member '%s' is a function and the declared one is a variable." c_name i_name decl_name in
              let help_msg = Printf.sprintf "Change the definition of '%s' to a variable." decl_name in
              semantic_error def_loc err_msg help_msg
            | _ -> ignore_case 7 "In the definitions table only variables and functions are allowed."
          end
        with
          Symbol_table.NotFound(_) -> 
            let err_msg = Printf.sprintf "Component '%s' provides the interface '%s', but the declared variable '%s' in the interface is not defined in the component." c_name i_name decl_name in
            let help_msg = Printf.sprintf "Define the variable '%s' in the component '%s'." decl_name c_name in
            semantic_error decl_loc err_msg help_msg
      end
    (* Check that the component defines the Function that has been declared in the interface and the signature matches *)
    | FunctionSymbol((_, decl_type, decl_loc), _) ->
      begin
        try
          let def_symbol = Symbol_table.lookup decl_name definitions_table in
          begin
            match def_symbol with
            | FunctionSymbol((_, def_type, def_loc), _) -> 
              let is_defined_correctly = decl_type = def_type in
              if is_defined_correctly then () else
                let err_msg = Printf.sprintf "Component '%s' provides the interface '%s', but the defined function '%s' has a different signature from the declared one." c_name i_name decl_name in
                let help_msg = Printf.sprintf "Change the signature of '%s' from '%s' to '%s'." decl_name (Ast.show_typ def_type) (Ast.show_typ decl_type) in
                semantic_error def_loc err_msg help_msg
            | VarSymbol((_, _, def_loc)) -> 
              let err_msg = Printf.sprintf "Component '%s' provides the interface '%s', but the defined member '%s' is a variable and the declared one is a function." c_name i_name decl_name in
              let help_msg = Printf.sprintf "Change the definition of '%s' to a function." decl_name in
              semantic_error def_loc err_msg help_msg
            | _ -> ignore_case 8 "In the definitions table only variables and functions are allowed."
          end
        with 
          Symbol_table.NotFound(_) -> 
            let err_msg = Printf.sprintf "Component '%s' provides the interface '%s', but the declared function '%s' in the interface is not defined in the component." c_name i_name decl_name in
            let help_msg = Printf.sprintf "Define the function '%s' in the component '%s'." decl_name c_name in
            semantic_error decl_loc err_msg help_msg
      end
    | _ -> ignore_case 9 "In the declarations table only variables and functions are allowed."


   (**
    Check that a component defines all the variables and functions
    that are declared in a given interface.
    @param c_name The name of the component.
    @param i_name The name of the interface.
    @param c_definitions_table The table of the component definitions.
    @param i_declaration_table The table of the interface declarations.
  *)
  let check_component_implements_provided_interface c_name i_name c_definitions_table i_declarations_table = 
    let check = check_component_implements_provided_declaration c_name i_name c_definitions_table in
    Symbol_table.iter check i_declarations_table


  (**
    Check that a component does not define a variable or a function that matches with
    a declaration of one of the used interface.
    @param c_name The name of the component.
    @param i_name The name of the interface.
    @param definitions_table The table of the component definitions.
    @param decl_name The name of the declaration.
    @param decl_symbol The symbol of the declaration.
    @raise SemanticError if the component defines a variable/function that matches the declaration.
  *)
  let check_component_use_of_declaration c_name i_name definitions_table decl_name decl_symbol = 
    match decl_symbol with
    (* Check that the defined variable in the component doesn't have the same name of a variable/function in the used interface *)
    | VarSymbol(_) -> 
      begin
        try
        let def_symbol = Symbol_table.lookup decl_name definitions_table in
        match def_symbol with
        | VarSymbol((_, _, def_loc)) -> 
          let err_msg = Printf.sprintf "The variable '%s' in the component '%s' is already defined in the used interface '%s'." decl_name c_name i_name in
          let help_msg = Printf.sprintf "Rename the variable '%s' in the component '%s'." decl_name c_name in
          semantic_error def_loc err_msg help_msg
        | FunctionSymbol((_, _, def_loc), _) -> 
          let err_msg = Printf.sprintf "The function '%s' in the component '%s' is already defined in the used interface '%s' as a variable'." decl_name c_name i_name in
          let help_msg = Printf.sprintf "Rename the function '%s' in the component '%s'." decl_name c_name in
          semantic_error def_loc err_msg help_msg
        | _ -> ignore_case 4 "In the definitions table only variables and functions are allowed."
        with 
          Symbol_table.NotFound(_) -> () 
      end
    (* Check that the defined function in the component doesn't have the same name of a variable/function in the used interface *)
    | FunctionSymbol(_) ->
      begin
        try
          let def_symbol = Symbol_table.lookup decl_name definitions_table in
          match def_symbol with
          | VarSymbol((_, _, def_loc)) -> 
            let err_msg = Printf.sprintf "The variable '%s' in the component '%s' is already defined in the used interface '%s' as a function." decl_name c_name i_name in
            let help_msg = Printf.sprintf "Rename the variable '%s' in the component '%s'." decl_name c_name in
            semantic_error def_loc err_msg help_msg
          | FunctionSymbol((_, _, def_loc), _) -> 
            let err_msg = Printf.sprintf "The function '%s' in the component '%s' is already defined in the used interface '%s'." decl_name c_name i_name in
            let help_msg = Printf.sprintf "Rename the function '%s' in the component '%s'." decl_name c_name in
            semantic_error def_loc err_msg help_msg
          | _ -> ignore_case 5 "In the definitions table only variables and functions are allowed."
        with 
          Symbol_table.NotFound(_) -> ()
      end
    | _ -> ignore_case 6 "In the declarations table only variables and functions are allowed."


  (**
    Check that a component does not define a variable or a function
    that is already declared in a given interface.
    @param c_name The name of the component.
    @param i_name The name of the interface.
    @param c_definitions_table The table of the component definitions.
    @param i_declaration_table The table of the interface declarations.
  *)
  let check_component_use_of_interface c_name i_name c_definitions_table i_declarations_table = 
    let check = check_component_use_of_declaration c_name i_name c_definitions_table in
    Symbol_table.iter check i_declarations_table
  

  (**
    Visit the list of used interfaces by a component and keeps track of them
    in the used table. 
    @param c_definitions_table the definition table of the component
    @param interfaces_table the table of interfaces
    @param c_name the component name
    @param loc the location of the provided interface
    @param uses_table the symbol table to update
    @param i_name the interface name
    @return the updated symbol table
    @raise Semantic_error if the interface is not defined, if it is already used,
      of if it's the App interface.
  *)
  let visit_use c_definitions_table interfaces_table c_name loc uses_table i_name =
    if i_name = "App" then
      let err_msg = Printf.sprintf "Components cannot use the 'App' interface, it can only be provided." in
      let help_msg = Printf.sprintf "Remove the use of the interface '%s' from the component '%s'." i_name c_name in
      semantic_error loc err_msg help_msg
    else try 
      let symbol = Symbol_table.lookup i_name interfaces_table in
      match symbol with
      | InterfaceSymbol(_, i_declarations_table) -> 
        check_component_use_of_interface c_name i_name c_definitions_table i_declarations_table;
        Symbol_table.add_entry i_name symbol uses_table
      | _ -> ignore_case 3 "In the interfaces table only interfaces are allowed."
    with 
    | Symbol_table.NotFound(_) ->
      let err_msg = Printf.sprintf "The component '%s' uses the interface '%s', which is not defined." c_name i_name in
      let help_msg = "Check if you have misspelled the interface name, otherwise create it." in
      semantic_error loc err_msg help_msg
    | Symbol_table.DuplicateEntry(id) ->
      let err_msg = Printf.sprintf "The component '%s' uses the interface '%s' more than once." c_name id in
      let help_msg = "Remove the duplicates in the uses list." in
      semantic_error loc err_msg help_msg

  
  (**
    Visit the list of provided interfaces by a component and keeps track of them
    in the provided table. 
    @param c_definitions_table the definition table of the component
    @param interfaces_table the table of interfaces
    @param c_name the component name
    @param loc the location of the provided interface
    @param provides_table the symbol table to update
    @param i_name the interface name
    @return the updated symbol table
    @raise Semantic_error if the interface is not defined, if it is already provided,
      of if it's the Prelude interface.
  *)
  let visit_provide c_definitions_table interfaces_table c_name loc provides_table i_name =
    (* Check that Prelude from the StdLib is not provided *)
    if i_name = "Prelude" then
      let err_msg = Printf.sprintf "Components cannot provide the 'Prelude' interface, it can only be used." in
      let help_msg = Printf.sprintf "Remove the provide of the interface '%s' from the component '%s'." i_name c_name in
      semantic_error loc err_msg help_msg
    else try 
      let symbol = Symbol_table.lookup i_name interfaces_table in
      match symbol with
      | InterfaceSymbol(_, i_declarations_table) -> 
        check_component_implements_provided_interface c_name i_name c_definitions_table i_declarations_table;
        Symbol_table.add_entry i_name symbol provides_table
      | _ -> ignore_case 2 "In the interfaces table only interfaces are allowed."
    with 
    | Symbol_table.NotFound(_) ->
      let err_msg = Printf.sprintf "The component '%s' provides the interface '%s', which is not defined." c_name i_name in
      let help_msg = "Check if you have misspelled the interface name, otherwise create it." in
      semantic_error loc err_msg help_msg
    | Symbol_table.DuplicateEntry(id) ->
      let err_msg = Printf.sprintf "The component '%s' provides the interface '%s' more than once." c_name id in
      let help_msg = "Remove the duplicates in the provides list." in
      semantic_error loc err_msg help_msg
  
  
  (** 
    Given a formal parameter of a function and the function symbol table,
    checks that the type is a valid one and that the parameter is not already defined.
    @param loc the location of the formal parameter
    @param function_table the symbol table of the function
    @param formal the formal parameter to check
    @return the updated function table
    @raise Semantic_error if the formal parameter is not valid or already defined
  *)
  let visit_formal loc function_table formal =
    let (identifier, typ) = formal in
    match typ with
    (* Forbid parameters with void type ->                             def example(my_par : void, ...) *)
    | Ast.TVoid ->
      let err_msg = Printf.sprintf "The parameter '%s' cannot have type 'void'." identifier in
      let help_msg = "'void' can only be used as a return type of a function." in
      semantic_error loc err_msg help_msg
    (* Forbid parameters with arrays of void type ->                   def example(my_par : void[], ...) *)
    | Ast.TArray(Ast.TVoid, _) ->
      let err_msg = Printf.sprintf "The parameter '%s' cannot have type 'void[]'." identifier in
      let help_msg = "'void' can only be used as a return type of a function." in
      semantic_error loc err_msg help_msg
    (* Forbid parameter with multi-dimensional arrays of any type ->   def example(my_par : int[][][], ...) *)
    | Ast.TArray(Ast.TArray(_, _), _) ->
      let err_msg = Printf.sprintf "The parameter '%s' cannot have a multi-dimensional array type." identifier in
      let help_msg = "Multi-dimensional arrays are not supported." in
      semantic_error loc err_msg help_msg
    (* Forbid parameters with arrays of a fixed size ->                def example(my_par : int[5], ...) *)
    | Ast.TArray(_, Some(n)) ->
      let err_msg = Printf.sprintf "The parameter '%s' is a reference to an array with a fixed size." identifier in
      let help_msg = Printf.sprintf "Size is not needed. Remove the size '%d' in the brackets." n in
      semantic_error loc err_msg help_msg
    (* Forbid parameters with a non primitive reference ->             def example(my_par : &void, ...) *)
    | Ast.TRef(typ) when not(Ast.is_primitive typ) ->
      let err_msg = Printf.sprintf "Variable '%s' cannot be a reference to a non-primitive type." identifier in
      let help_msg = "References can only be used with primitive types (i.e. int, bool, char)." in
      semantic_error loc err_msg help_msg
    (* Forbid parameters with a non primitive reference ->             def example(my_par : &int[], ...) *)
    | Ast.TArray(Ast.TRef(_), _) -> 
      let err_msg = Printf.sprintf "Variable '%s' cannot be a reference to an array." identifier in
      let help_msg = "References can only be used with primitive types (i.e. int, bool, char)." in
      semantic_error loc err_msg help_msg
    | _ ->
      (* Check if already defined in the function parameter list *)
      begin
        let info = (identifier, typ, loc) in
        let value = VarSymbol(info) in
        try 
          Symbol_table.add_entry identifier value function_table
        with
        | Symbol_table.DuplicateEntry(id) ->
          let err_msg = Printf.sprintf "The parameter '%s' is declared more than once." id in
          let help_msg = "Remove the duplicates in the parameters list." in
          semantic_error loc err_msg help_msg
      end


  (**
    Given a definition (e.g. variable or function) of a component and the definition
    table, check it and add it to the definition table to keep track of it.
    @param c_name the name of the component
    @param definitions_table the table of definitions of the component
    @param definition the definition to check and add
    @return the updated definition table
    @raise Semantic_error if the definition has been already defined
  *)
  let visit_definition c_name definitions_table definition =
    let loc = definition.annot in 
    match definition.node with
    | Ast.FunDecl {rtype; fname; formals; _} -> 
      begin
        InterfaceVisitor.is_function_return_type_legal rtype fname loc;
        let function_table = List.fold_left (visit_formal loc) (Symbol_table.empty_table ()) formals in
        let value = FunctionSymbol((fname, Ast.TFun (List.map snd formals, rtype), loc), function_table) in
        try
          Symbol_table.add_entry fname value definitions_table
        with
        | Symbol_table.DuplicateEntry(id) ->
          let err_msg = Printf.sprintf "The component '%s' defined multiple times the function '%s'." c_name id in
          let help_msg = "Functions must have unique names. Rename it or remove it." in
          semantic_error loc err_msg help_msg
      end
    | Ast.VarDecl(identifier, typ) ->
      begin
        InterfaceVisitor.is_variable_decl_legal identifier typ loc;
        let info = (identifier, typ, loc) in
        let value = VarSymbol(info) in
        try
          Symbol_table.add_entry identifier value definitions_table
        with
        | Symbol_table.DuplicateEntry(id) ->
          let err_msg = Printf.sprintf "The component '%s' defined multiple times the variable '%s'." c_name id in
          let help_msg = "Variables must have unique names. Rename it or remove it." in
          semantic_error loc err_msg help_msg
      end
  
  
  (**
    Given the initial global_table and an initial counter to keep track of
    the number of components that provides the "App" interface from StdLibrary,
    and a component decl, it visit the component to update the global_table
    and it returns it together with the updated counter.
    @param output the global table and the counter
    @param component_decl the component decl to visit
    @return the updated global table and the updated counter
    @raise Semantic_error if the component is defined more than once
  *)
  let visit_component output c_decl = 
    let is_app_provided_by_component provides_table =
      try let _ = Symbol_table.lookup "App" provides_table in 1
      with Symbol_table.NotFound(_) -> 0
    in
    
    let (table, app_provided_counter) = output in
    let loc = c_decl.annot in
    match c_decl.node with Ast.ComponentDecl{ cname; uses; provides; definitions } ->
      let definitions_table = List.fold_left (visit_definition cname) (Symbol_table.empty_table ()) definitions in
      let uses_table = List.fold_left (visit_use definitions_table table.interfaces cname loc) (Symbol_table.empty_table ()) uses in
      let provides_table = List.fold_left (visit_provide definitions_table table.interfaces cname loc) (Symbol_table.empty_table ()) provides in
      let app_provided_counter = app_provided_counter + is_app_provided_by_component provides_table in
      
      (* DEBUG *)
      if debug_mode then
        let uses_out =  Symbol_table.show uses_table in
        let provides_out = Symbol_table.show provides_table in
        let component_out = Symbol_table.show definitions_table in
        Printf.printf "\nCOMPONENT: %s\n" cname; 
        Printf.printf " Uses table: %s\n" uses_out;
        Printf.printf " Provides table: %s\n" provides_out;
        Printf.printf " Definitions table: %s\n" component_out;
      else ();
      (* END-DEBUG *)
     
      (* TODO: decide weather cname should be saved in info given that it's already in the value *)
      (* TODO: do a copy of semantic_analysis and remove everywhere the name *)
      let info = (cname, Ast.TComponent(cname), loc) in
      let value = ComponentSymbol(info, uses_table, provides_table, definitions_table) in
      try
        let _ = Symbol_table.add_entry cname value table.components in
        (table, app_provided_counter)
      with
      | Symbol_table.DuplicateEntry(id) ->
        let err_msg = Printf.sprintf "The component '%s' is defined more than once." id in
        let help_msg = "Components must have unique names. Rename it." in
        semantic_error loc err_msg help_msg


  (**
    The main function of the ComponentVisitor module. It visits the components
    definitions and returns the updated component table and the components using
    the Prelude interface.
    @param table The global table to be updated.
    @param components The list of components to be visited.
    @return The updated global table and the new list of components that uses implicitly the Prelude interface.
    @raise SemanticError if the App interface is not provided by one and only one component.
  *)
  let rec visit_components table components = 
    (* Every component implicitly uses the interface Prelude from the StdLibrary (the one that provides print(int) and get() -> int) *)
    let components = List.map add_prelude_to_component components in
    (* Visit all the components and check that only one component provides the "App" interface from the StdLibrary *)
    let app_provided_counter = 0 in 
    let (table, app_provided_counter) = List.fold_left visit_component (table, app_provided_counter) components in
    
    if app_provided_counter = 0 then
      let err_msg = "No component provides the 'App' interface." in
      let help_msg = "Add the 'App' in the used interfaces of one component to define the main function." in
      semantic_error Location.dummy_code_pos err_msg help_msg
    else if app_provided_counter > 1 then
      let err_msg = "Multiple components provides the 'App' interface." in
      let help_msg = "Only one component must provide the 'App' in the used interfaces to have one single entrypoint." in
      semantic_error Location.dummy_code_pos err_msg help_msg
    else (table, components)

  and add_prelude_to_component component = 
    match component.node with Ast.ComponentDecl{ cname; uses; provides; definitions } ->
      let new_node = Ast.ComponentDecl{ cname; uses="Prelude" :: uses; provides; definitions } in
      { component with node = new_node }
      
end
(* END COMPONENT VISITOR SUB-MODULE*)



(* ================================== 2nd VISIT - TYPING RULES ==================================*)


module TypeAnalysis = struct
  (* COMPONENTS *)

  (**
    Given an expression, it returns it if the type is boolean, otherwise raise
    a semantic error.
    @param expr The expression to check.
    @param loc The location of the expression.
    @return The expression if the type is boolean.
    @raise SemanticError if the type is not boolean.
  *)
  let check_expr_bool expr loc = 
    match expr.annot with
    | Ast.TBool -> expr
    | _ -> 
      let err_msg = "The expression must be of type 'bool'." in
      let help_msg = Printf.sprintf "The expression has type '%s'." (Ast.show_typ expr.annot) in
      semantic_error loc err_msg help_msg
  
  
  (**
    Given a binary operator and its operands, check weather the operation is valid.
    @param binop The binary operator.
    @param a The left operand.
    @param b The right operand.
    @param loc The location of the expression.
    @raise SemanticError if the operation is not valid.
  *)
  let is_binary_op_valid binop a b loc =
    let t1 = a.annot in
    let t2 = b.annot in
    match (binop, a.annot, b.annot) with
    (* Math operators with operands of type int or &int *)
    | _, _, _ when Ast.is_math_op(binop) && Ast.is_of_typ_or_reftyp Ast.TInt t1 && Ast.is_of_typ_or_reftyp Ast.TInt t2 -> Ast.TInt
    (* Boolean operators with operands of type bool or &bool *)
    | _, _, _ when Ast.is_bool_op(binop) && Ast.is_of_typ_or_reftyp Ast.TBool t1 && Ast.is_of_typ_or_reftyp Ast.TBool t2 -> Ast.TBool
    (* Comparison operators with operands of type (int or &int) xor (bool or &bool) *)
    | _, _, _ when Ast.is_comp_op(binop) && ((Ast.is_of_typ_or_reftyp Ast.TInt t1 && Ast.is_of_typ_or_reftyp Ast.TInt t2) ||
                                             (Ast.is_of_typ_or_reftyp Ast.TBool t1 && Ast.is_of_typ_or_reftyp Ast.TBool t2)) -> Ast.TBool
    | _ ->
      let err_msg = Printf.sprintf "The binary operator '%s' is not defined for the types '%s' and '%s'." (Ast.show_binop binop) (Ast.show_typ t1) (Ast.show_typ t2) in
      let help_msg = Printf.sprintf "The operands must be equal of equal type or reference type." in
      semantic_error loc err_msg help_msg

  
  (**
    Given a unary operator and it operand, check weather the operation is valid.
    @param uop The unary operator.
    @param a The operand.
    @param loc The location of the expression.
    @raise SemanticError if the operation is not valid.
  *)
  let is_unary_op_valid uop a loc =
    match (uop, a.annot) with
    | (Ast.Neg, _) when Ast.is_of_typ_or_reftyp Ast.TInt a.annot -> Ast.TInt
    | (Ast.Neg, _) -> raise (Semantic_error(loc, "Unary 'negation' can only be applied to integers."))
    | (Ast.Not, _) when Ast.is_of_typ_or_reftyp Ast.TBool a.annot -> Ast.TBool
    | (Ast.Not, _) -> raise (Semantic_error(loc, "Unary 'not' can only be applied to booleans."))
  

  (**
    Given an assignment of an expression to a lvalue,
    check weather the operation has expected type.
    @param lvalue The lvalue to assign.
    @param expr The expression to assign.
    @param loc The location of the expression.
    @raise SemanticError if the assignment is not valid.
  *)
  let is_assignment_legal lvalue expr loc =
    match (lvalue.annot, expr.annot) with
    (* Same primitive type *)
    | t1, t2 when (Ast.equal_typ t1 t2) && (Ast.is_primitive t1) -> ()
    (* Same primitive reference type *)
    | Ast.TRef(t1), Ast.TRef(t2) when (Ast.equal_typ t1 t2) && (Ast.is_primitive t1) -> ()
    (* Left primitive reference type and right primitive type *)
    | Ast.TRef(t1), t2 when (Ast.equal_typ t1 t2) && (Ast.is_primitive t1) -> ()
    (* Right primitive type and left primitive reference type *)
    | t1, Ast.TRef(t2) when (Ast.equal_typ t1 t2) && (Ast.is_primitive t1) -> ()
    (* Forbid array assignment *)
    | Ast.TArray(_), Ast.TArray(_) -> 
      let err_msg = Printf.sprintf "Array assignment is not supported yet." in
      let help_msg = Printf.sprintf "Use a loop to assign each element of the array." in
      semantic_error loc err_msg help_msg
    (* Forbid void assignment *)
    | _, Ast.TVoid ->
      let err_msg = Printf.sprintf "Cannot assign a 'void' expression to an lvalue." in
      let help_msg = Printf.sprintf "Use a return statement to return a value." in
      semantic_error loc err_msg help_msg
    | _, _ -> 
    let err_msg = Printf.sprintf "Cannot assign '%s' to '%s'." (Ast.show_typ expr.annot) (Ast.show_typ lvalue.annot) in
    let help_msg = "The types of the left and right hand side of the assignment must be the same." in
    semantic_error loc err_msg help_msg


  (**
    Given a type and a location, check weather the type is valid for the increment and decrement operators.
    @param t The type to check.
    @param loc The location of the expression.
    @raise SemanticError if the type is not valid.
  *)
  let is_inc_dec_valid t loc =
    match t with
    | Ast.TInt -> ()
    | _ -> 
      let err_msg = Printf.sprintf "The increment and decrement operators can only be applied to integers." in
      let help_msg = Printf.sprintf "You are using the operator on a '%s'." (Ast.show_typ t) in
      semantic_error loc err_msg help_msg

  
  (**
      Given the environment and the lvalue, return the type annotated lvalue.
      @param env The environment.
      @param lvalue The lvalue to annotate.
      @return The annotated lvalue.
      @raise SemanticError if the lvalue is not valid.
  *)
  let rec annotate_lvalue env lvalue = 
    let function_table = env.current_table in
    let component = env.component in
    let component_symbol = env.component_symbol in
    let loc = lvalue.annot in

    let check_in_table interface id table = 
      let symbol = Symbol_table.lookup id table in
      match symbol with
      | VarSymbol(info) -> 
        let (id, typ, _) = info in Ast.annotate_node (Ast.AccVar(interface, id)) typ
      | FunctionSymbol(_) -> raise (Semantic_error(loc, "Function used as lvalue"))
      | _ -> ignore_case 13 "In the function table only variable and function symbols are allowed."
    in
    
    match lvalue.node with
    (* Check variable assignment *)
    | Ast.AccVar(Some(_), _) -> ignore_case 14 "The first optional identifier, must be defined here, in the second pass."
    | Ast.AccVar(None, id) ->
      let (_, uses) = match component with Ast.ComponentDecl{ cname; uses; _ } -> (cname, uses) in
      let (uses_table, definitions_table) = match component_symbol with ComponentSymbol(_, uses, _, definitions_table) -> uses, definitions_table | _ -> ignore 18 in
      begin
        try
          (* 1. Check if access to a variable on the function scope *)
          check_in_table None id function_table
        with Symbol_table.NotFound(id) -> 
          begin
            try 
              (* 2. Check if access to a variable on the current component scope *)
              check_in_table None id definitions_table
            with Symbol_table.NotFound(_) -> 
              (* 3. Check if access to a variable on any used interface *)
              let rec check_in_used_interface uses = 
                match uses with
                | interface_name :: uses -> 
                  let interface_symbol = Symbol_table.lookup interface_name uses_table in
                  let interface_table = match interface_symbol with InterfaceSymbol(_, interface_table) -> interface_table | _ -> ignore 19 in
                  begin
                  try 
                    check_in_table (Some(interface_name)) id interface_table
                  with Symbol_table.NotFound(_) -> check_in_used_interface uses
                  end
                | [] -> 
                  (* 4. The variable is not defined *)
                  let err_msg = Printf.sprintf "The identifier '%s' is not defined in the current scope nor in the used interfaces." id in
                  let help_mg = Printf.sprintf "Variables must be declared with the 'var' keyword." in
                  semantic_error loc err_msg help_mg
                in check_in_used_interface uses
          end
      end
    (* Check array assignment *)
    | Ast.AccIndex(l, expr) -> 
      let te = annotate_expr env expr in
      match te.annot with 
      | Ast.TInt -> 
        begin
          let tl = annotate_lvalue env l in
          match tl.annot with 
          | Ast.TArray(Ast.TRef(typ), _) ->
            if Ast.is_primitive(typ) then
              Ast.AccIndex(tl, te) ++ typ
            else 
              let err_msg = Printf.sprintf "The lvalue has type '%s', but only scalar type can be accessed with an index." (Ast.show_typ te.annot) in
              let help_msg = "The lvalue must be an array or an array references." in
              semantic_error loc err_msg help_msg
          | Ast.TArray(typ, _) -> 
            if Ast.is_primitive(typ) then
              Ast.AccIndex(tl, te) ++ typ
            else 
              let err_msg = Printf.sprintf "The lvalue has type '%s', but only scalar type can be accessed with an index." (Ast.show_typ te.annot) in
              let help_msg = "The lvalue must be an array or an array references." in 
              semantic_error loc err_msg help_msg
          (* Forbid access over a primitive type *)
          | _ -> 
            let err_msg = Printf.sprintf "The lvalue of a '%s' type cannot be accessed by an index." (Ast.show_typ tl.annot) in 
            let help_msg = Printf.sprintf "Only an array or an array references are allowed to do so." in
            semantic_error loc err_msg help_msg
        end
      (* Forbid access with a non-integer index *)
      | _ -> 
        let err_msg = Printf.sprintf "The index of an array must be an 'int', but it has type '%s'." (Ast.show_typ te.annot) in
        let help_msg = "The index of an array must be an 'int'." in
        semantic_error loc err_msg help_msg

  (**
    Given the environment and the expression, return the type annotated expression.
    @param env The environment.
    @param expr The expression to annotate.
    @return The annotated expression.
  *)
  and annotate_expr env expr =
    let loc = expr.annot in 
    match expr.node with
    | Ast.LV(lvalue) ->
      let tl = annotate_lvalue env lvalue in
      Ast.LV(tl) ++ tl.annot
    | Ast.Assign(lvalue, expr) ->
      let tl = annotate_lvalue env lvalue in
      let te = annotate_expr env expr in
      is_assignment_legal tl te loc;
      Ast.Assign(tl, te) ++ tl.annot
    | Ast.ILiteral(i) -> Ast.ILiteral(i) ++ Ast.TInt
    | Ast.CLiteral(c) -> Ast.CLiteral(c) ++ Ast.TChar
    | Ast.BLiteral(b) -> Ast.BLiteral(b) ++ Ast.TBool
    | Ast.UnaryOp(uop, e) -> 
      let te = annotate_expr env e in
      let typ = is_unary_op_valid uop te loc in Ast.UnaryOp(uop, te) ++ typ
    | Ast.Address(lvalue) -> 
      let tl = annotate_lvalue env lvalue in
      Ast.Address(tl) ++ Ast.TRef(tl.annot)
    | Ast.BinaryOp(binop, e1, e2) ->
      let te1 = annotate_expr env e1 in
      let te2 = annotate_expr env e2 in
      let binop_typ = is_binary_op_valid binop te1 te2 loc in 
      Ast.BinaryOp(binop, te1, te2) ++ binop_typ
    | Ast.Call(_, i2, exprs) ->
      let te1 = List.map (annotate_expr env) exprs in
      annotate_call env i2 te1 loc
    | Ast.IncDec(lvalue, inc_dec, pre_post) ->
      let tl = annotate_lvalue env lvalue in
      let typ = is_inc_dec_valid tl.annot loc in
      Ast.IncDec(tl, inc_dec, pre_post) ++ tl.annot

  (**
    Given the environment,the function name and list of arguments,
    check if the call is valid and annotate it.
    @param env The environment.
    @param fname The function name.
    @param texprs The list of typed arguments.
    @param loc The location of the call.
    @return The annotated call.
    @raise SemanticError if the call is not valid.
  *)
  and annotate_call env fname texprs loc = 
    let (_, uses) = match env.component with Ast.ComponentDecl{ cname; uses; _ } -> (cname, uses) in
    let component_symbol = env.component_symbol in
    let (uses_table, definitions_table) = match component_symbol with ComponentSymbol(_, uses_table, _, definitions_table) -> uses_table, definitions_table | _ -> ignore 20 in
    
    let check_function iname function_info = 
      let (_, typ, _) = function_info in
      match typ with
      | Ast.TFun(parameters, rtype) ->
        let argument_types = List.map (fun e -> e.annot) texprs in
        (* If the type is an array, then remove the size, because parameters are declared without it *)
        let argument_types = List.map (fun t -> match t with Ast.TArray(typ, _) -> Ast.TArray(typ, None) | _ -> t) argument_types in
        (* Check that arguments are equal to the required parameters *)
        if List.length argument_types = List.length parameters then
          let rec check_arguments arguments parameters counter = 
            match arguments, parameters with
            | [], [] -> ()
            | a :: arguments, p :: parameters when Ast.equal_typ a p -> check_arguments arguments parameters (counter + 1)
            | Ast.TRef(a) :: arguments, p :: parameters when Ast.equal_typ a p -> check_arguments arguments parameters (counter + 1)
            | a :: _, p :: _ when not (Ast.equal_typ a p) -> 
              let err_msg = Printf.sprintf "The argument n. %d has the wrong type." counter in
              let help_msg = Printf.sprintf "The provided argument has type '%s', but the parameter has an expected type '%s'." (Ast.show_typ a) (Ast.show_typ p) in
              semantic_error loc err_msg help_msg
            | _, _ -> ignore_case 15 "The number of arguments and parameters is the same."
          in check_arguments argument_types parameters 1;
          (* Return the annotated expression *)
          Ast.Call(iname, fname, texprs) ++ rtype
        (* The length of arguments and parameters does not match *)
        else
          let err_msg = "The number of arguments does not match the number of parameters." in
          let help_msg = Printf.sprintf "The function '%s' requires %d argument(s) but %d were given." fname (List.length parameters) (List.length argument_types) in
          semantic_error loc err_msg help_msg
      | _ -> 
        raise (Semantic_error(loc, "This is not a function."))
    in

    let check_in_table iname id table = 
      let symbol = Symbol_table.lookup id table in
      match symbol with
      | FunctionSymbol(function_info, _) -> 
          check_function iname function_info
      | VarSymbol(_) -> raise (Semantic_error(loc, "This is a variable not a function."))
      | _ -> ignore_case 16 "In the definition table only definitions (fun and var) are allowed."
    in

    let rec check_used_interfaces uses = 
      match uses with
      | iname :: tail -> 
        begin
          try 
            let symbol = Symbol_table.lookup iname uses_table in
            match symbol with
            | InterfaceSymbol((_, _, _), declarations_table) -> 
              check_in_table (Some(iname)) fname declarations_table
            | _ -> ignore_case 17 "In the uses table only interfaces are allowed."
          with
          | Symbol_table.NotFound(_) -> check_used_interfaces tail
        end
        | [] ->
          let err_msg = Printf.sprintf "The used function '%s' is not defined." fname in
          let help_msg = "Check the name of the function or define it." in
          semantic_error loc err_msg help_msg
    in

    try 
      (* Check that the functoin is available in the definitions of the current component. *)
      check_in_table None fname definitions_table
    with
    | Symbol_table.NotFound(_) -> 
      (* If missing, check that the function is defined in one of the used interfaces. *)
      check_used_interfaces uses

  
  (**
    Given a statement, check it and annotate with Ast.TVoid.
    @param env the environment
    @param rtype the return type of the function
    @param stmt the statement to annotate
    @return the annotated statement
    @raise Semantic_error if the statement is not valid
  *)
  let rec annotate_stmt env rtype stmt =
    let function_table = env.current_table in 
    let loc = stmt.annot in
    let node = match stmt.node with 
    | Ast.If(e, s1, s2) -> 
      let te = check_expr_bool (annotate_expr env e) loc in
      let if_table = Symbol_table.begin_block function_table in
      let env = { env with current_table = if_table } in
      let ts1 = annotate_stmt env rtype s1 in
      let ts2 = annotate_stmt env rtype s2  in
      Ast.If(te, ts1, ts2)

    | Ast.While(e, s1) ->
      let te = check_expr_bool (annotate_expr env e) loc in 
      let while_table = Symbol_table.begin_block function_table in
      let env = { env with current_table = while_table } in
      let ts1 = annotate_stmt env rtype s1 in
      Ast.While(te, ts1)

    | Ast.DoWhile(e, s) -> 
      let dowhile_table = Symbol_table.begin_block function_table in
      let env = { env with current_table = dowhile_table } in
      let ts = annotate_stmt env rtype s in
      let te = check_expr_bool (annotate_expr env e) loc in 
      Ast.DoWhile(te, ts)

    | Ast.For(e1, e2, e3, s) -> 
      let for_table = Symbol_table.begin_block function_table in
      let env = { env with current_table = for_table } in
      let te1 = match e1 with None -> None | Some(e) -> Some(annotate_expr env e) in
      (* If the second expression of the FOR is not there, this means loop forever.
        Otherwise use the provided expression to determine the stopping condition *)
      let always_true = Ast.BLiteral(true) ++ Ast.TBool in
      let te2 = match e2 with None -> Some(always_true) | Some(e) -> Some(check_expr_bool (annotate_expr env e) loc) in
      let te3 = match e3 with None -> None | Some(e) -> Some(annotate_expr env e) in
      let ts = annotate_stmt env rtype s in
      Ast.For(te1, te2, te3, ts)

    | Ast.Expr(e) ->
      let te = annotate_expr env e in 
      Ast.Expr(te)

    | Ast.Return(None) ->
        if rtype = Ast.TVoid then Ast.Return(None)
        else
          let err_msg = "The function must return a value but the return statement is empty." in
          let help_msg = Printf.sprintf "The function has return type '%s'." (Ast.show_typ rtype) in
          semantic_error loc err_msg help_msg

    | Ast.Return(Some(e)) ->
        let te = annotate_expr env e in
        if te.annot = rtype 
          then Ast.Return(Some(te))
        else 
          let err_msg = "The return statement type does not match the function return type." in
          let help_msg = Printf.sprintf "The function has return type '%s' but it was expected type '%s'." (Ast.show_typ rtype) (Ast.show_typ te.annot) in
          semantic_error loc err_msg help_msg

    | Ast.Block(stmtordec) -> 
      (* For each stmtordec in the list stmordec, create a new block in the symboltable *)
      let block_table = Symbol_table.begin_block function_table in
      let env = { env with current_table = block_table } in
      let tstmtordec = List.map (annotate_stmtordec env rtype) stmtordec in
      Ast.Block(tstmtordec)

    | Ast.Skip -> Ast.Skip
    in 
    node ++ Ast.TVoid

  and annotate_stmtordec env rtype stmtordec = 
    let block_table = env.current_table in
    let loc = stmtordec.annot in
    match stmtordec.node with
    | Ast.LocalDecl((id, typ)) -> 
      begin
      InterfaceVisitor.is_variable_decl_legal id typ loc;
      try 
        let symbol = VarSymbol (id, typ, loc) in
        let _ = Symbol_table.add_entry id symbol block_table in
        Ast.LocalDecl((id, typ)) ++ typ
      with Symbol_table.DuplicateEntry(_) ->
        let err_msg = Printf.sprintf "The variable '%s' is already defined." id in
        let help_msg = "Check the name of the variable or define it." in
        semantic_error loc err_msg help_msg
      end
    | Ast.Stmt(s) -> 
      let ts = annotate_stmt env rtype s in
      Ast.Stmt(ts) ++ Ast.TVoid


  (**
    Given a list of definitions, annotate each definition with the proper type.
    @param component the component to which the definitions belong.
    @param component_symbol the component symbol
    @param definitions the list of definitions to annotate with the type
    @return the list of definitions annotated with the type 
  *)
  let annotate_definitions component component_symbol definitions = 
    let annotate_definition definition = 
      let component_table = match component_symbol with ComponentSymbol(_, _, _, ctable) -> ctable | _ -> ignore 10 in
      match definition.node with
      | Ast.FunDecl{ rtype; fname; formals; body } ->
        let annotated_body = match body with 
          | Some(stmt) ->
            let function_symbol = Symbol_table.lookup fname component_table in
            let function_table = match function_symbol with FunctionSymbol(_, table) -> table | _ -> ignore 11 in
            let env = {current_table = function_table; component; component_symbol} in
            annotate_stmt env rtype stmt
          | None -> ignore_case 12 "Definitions must have a body"
        in 
        let node = Ast.FunDecl{ rtype; fname; formals; body = Some(annotated_body) } in
        let annot = Ast.TFun(List.map snd formals, rtype) in
        node ++ annot
      | Ast.VarDecl(id, typ) ->
        Ast.VarDecl(id, typ) ++ typ
    in List.map annotate_definition definitions


  (**
    Given a list of components, annotate them with the proper type.
    @param global_table the global symbol table used to resolve symbols
    @param components the list of interfaces to annotate
    @return the list of type annotated components
  *)
  let annotate_components global_table components = 
    let annotate_component component = 
      match component.node with 
      | Ast.ComponentDecl{cname; uses; provides; definitions} ->
        let csymbol = Symbol_table.lookup cname global_table.components in
        let definitions = annotate_definitions component.node csymbol definitions in
        let node = Ast.ComponentDecl{ cname; uses; provides; definitions } in
        node ++ Ast.TComponent(cname)
    in List.map annotate_component components


  (* INTERFACES *)

  (**
    Given a list of declarations, annotate them with the proper type.
    @param declarations the list of declarations to annotate
    @return the list of type annotated declarations
  *)
  let annotate_declarations declarations = 
    let annotate_declaration declaration =
      match declaration.node with
      | Ast.FunDecl{ rtype; fname; formals; _ } ->
        let node = Ast.FunDecl{ rtype; fname; formals; body=None } in
        let annot = Ast.TFun(List.map snd formals, rtype) in
        node ++ annot
      | Ast.VarDecl(id, typ) ->
        Ast.VarDecl(id, typ) ++ typ
    in List.map annotate_declaration declarations 

  
  (**
    Given a list of interfaces, annotate them with the proper type.
    Note that this step could have been done in the IntefaceVisitor step,
    but in order to make it easier to understand is better to do it here.
    @param interfaces the list of interfaces to annotate
    @return the list of type annotated interfaces
  *)
  let annotate_interfaces interfaces = 
    let annotate_interface interface = 
      match interface.node with 
      | Ast.InterfaceDecl{ iname; declarations } ->
        let declarations = annotate_declarations declarations in
        let node = Ast.InterfaceDecl{ iname; declarations } in
        node ++ Ast.TInterface(iname)
    in List.map annotate_interface interfaces
end



(* ================================== MAIN FUNCTION ==================================*)

(**
  Given an AST with locations, it semantically analyzes the program and returns a new AST with types.
  @param ast the AST annotated with locations to be analyzed
  @return the annotated AST with types
*)
let type_check ast = 
  let (interfaces, components, cn) = match ast with Ast.CompilationUnit{interfaces=i; components=co; connections=cn} -> (i, co, cn) in 
  
  (* Add to the declared interfaces the one from the StandardLibrary *)
  let interfaces =  interfaces @ StandardLibrary.build_interfaces () in

  (* 1. Semantic analysis over components and interfaces - It's the first
    visit that does the first checks that can be made without a symbol table
    that tracks all the component and interfaces *)
  let global_table = { interfaces = Symbol_table.empty_table (); components = Symbol_table.empty_table ()} in
  let _ = InterfaceVisitor.visit_interfaces global_table.interfaces interfaces in
  let _, components = ComponentVisitor.visit_components global_table components in

  (* DEBUG *)
  if debug_mode then
    let interfaces_out = Symbol_table.show global_table.interfaces in
    let components_out = Symbol_table.show global_table.components in
    Printf.printf "\n=====GLOBAL TABLE=====\n";
    Printf.printf "\nINTERFACES TABLE:%s\n" interfaces_out;
    Printf.printf "\nCOMPONENTS TABLE:%s\n" components_out;
    Printf.printf "\n\n\n";
  else ();
  (* END-DEBUG *)

  (* 2. Type analysis - It's the second visit that uses the symbol table
    from the first visit, in order to continue with the checks *)
  let it = TypeAnalysis.annotate_interfaces interfaces in
  let ct = TypeAnalysis.annotate_components global_table components in

  (* Return -> Type checked abstract syntax tree *)
  Ast.CompilationUnit({interfaces=it; components=ct; connections=cn})


(* TODO: there is inconsitency with the help_msg, sometimes it's suggestion (do x) sometimes it's an error (expected y but it is z) *)
(* TODO: can I remove identifier from identifier_info type? *)
(* TODO: there is a bug with the location, if you have var arr : int, and var arr : void, it's going to talk about the latter but the pos will be the first one *)
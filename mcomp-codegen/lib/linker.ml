exception LinkingError of string

open Ast

(** Infix operator to annotate node with its type *)
let ( ++ ) a b = annotate_node a b

type link_table = (Ast.identifier, Ast.identifier) Hashtbl.t

type component_data =
  | ComponentData of {
      uses : Ast.identifier list;
      provides : Ast.identifier list;
      links : link_table;
    }

(** Helpers to raise Linking errors with an error message *)
let build_msg (err_msg : string) (help_msg : string) =
  err_msg ^ "\n    Help: " ^ help_msg

let linking_error err_msg help_msg =
  raise (LinkingError (build_msg err_msg help_msg))

module Checker = struct
  let show_link l_comp l_inter r_comp r_inter =
    l_comp ^ "." ^ l_inter ^ " <- " ^ r_comp ^ "." ^ r_inter

  let show_link link =
    match link with
    | Ast.Link (l_comp, l_inter, r_comp, r_inter) ->
        show_link l_comp l_inter r_comp r_inter

  let check_id_is_component component_table comp link =
    try
      let _ = Hashtbl.find component_table comp in
      ()
    with Not_found ->
      let err_msg =
        Printf.sprintf "The component '%s' in the link '{%s}' is not defined."
          comp (show_link link)
      in
      let help_msg =
        "Check if you have mispell the component name, otherwise define it."
      in
      linking_error err_msg help_msg

  let check_components_are_different l_comp r_comp link =
    if l_comp = r_comp then
      let err_msg =
        Printf.sprintf
          "The component '%s' is used on both sides of the link '{%s}'." l_comp
          (show_link link)
      in
      let help_msg = "Please remove one of the two components." in
      linking_error err_msg help_msg
    else ()

  let check_component_uses_interface table component interface link =
    let (ComponentData { uses; _ }) = Hashtbl.find table component in
    if List.mem interface uses then ()
    else
      let err_msg =
        Printf.sprintf
          "The interface '%s' is not used by the component '%s', but it's used \
           in the link '{%s}'."
          interface component (show_link link)
      in
      let help_msg =
        "Check if you have mispell the interface name, otherwise define it."
      in
      linking_error err_msg help_msg

  let check_component_provides_interface table component interface link =
    let (ComponentData { uses = _; provides; _ }) =
      Hashtbl.find table component
    in
    if List.mem interface provides then ()
    else
      let err_msg =
        Printf.sprintf
          "The interface '%s' is not provided by the component '%s', but it's \
           provided in the link '{%s}'."
          interface component (show_link link)
      in
      let help_msg =
        "Check if you have mispell the interface name, otherwise define it."
      in
      linking_error err_msg help_msg

  let check_interfaces_are_equal l_inter r_inter link =
    if l_inter <> r_inter then
      let err_msg =
        Printf.sprintf
          "The interfaces '%s' and '%s' in the link '{%s}' are different."
          l_inter r_inter (show_link link)
      in
      let help_msg = "Please make sure that the interfaces are the same." in
      linking_error err_msg help_msg
    else ()

  let visit_link component_table link =
    match link with
    (* Given a connection `ID1.ID2 <- ID3.ID4`, the linker must ensure that: *)
    | Ast.Link (l_comp, l_inter, r_comp, r_inter) -> (
        (* 1. ID1 and ID3 are the names of two different components; *)
        check_id_is_component component_table l_comp link;
        check_id_is_component component_table r_comp link;
        check_components_are_different l_comp r_comp link;
        (* 2. ID2 is the name of an interface used by ID1; *)
        check_component_uses_interface component_table l_comp l_inter link;
        (* 3. ID4 is the name of an interface provided by ID3. *)
        check_component_provides_interface component_table r_comp r_inter link;
        (* 4. ID2 and ID3 are the same interface. *)
        check_interfaces_are_equal l_inter r_inter link;

        let (ComponentData { uses = _; provides = _; links }) =
          Hashtbl.find component_table l_comp
        in
        match Hashtbl.find_opt links l_inter with
        (* Check if component is already linked *)
        | Some r_comp' ->
            if r_comp <> r_comp' then
              let err_msg =
                Printf.sprintf
                  "The connection '%s.%s' is already linked to the component \
                   '%s'."
                  l_comp l_inter r_comp'
              in
              let help_msg =
                "The used interface must be linked only to one component."
              in
              linking_error err_msg help_msg
            else
              let err_msg =
                Printf.sprintf
                  "The connection '%s.%s' is linked multiple times to '%s'."
                  l_comp l_inter r_comp'
              in
              let help_msg =
                "The used interface must be linked only to one component."
              in
              linking_error err_msg help_msg
        (* Otherwise keep track of the link *)
        | None -> Hashtbl.add links l_inter r_comp)

  let check_used_interfaces_are_provided components component_table =
    let check component =
      match component.node with
      | Ast.ComponentDecl { cname; uses = _; provides = _; definitions = _ }
        -> (
          let data = Hashtbl.find component_table cname in
          match data with
          | ComponentData { uses; provides = _; links } ->
              (* Check all uses of a specific component *)
              List.iter
                (fun interface ->
                  if interface = "Prelude" then ()
                  else
                    match Hashtbl.find_opt links interface with
                    | Some _ -> ()
                    | None ->
                        let err_msg =
                          Printf.sprintf
                            "The interface '%s' is used by the component '%s', \
                             but it's not provided."
                            interface cname
                        in
                        let help_msg =
                          Printf.sprintf
                            "In connect add a new link '%s.%s <- %s.%s'" cname
                            interface "<ProvidingComponent>" interface
                        in
                        linking_error err_msg help_msg)
                uses)
      (* Check all components *)
    in

    List.iter check components

  let visit_components components =
    let visit_component components_table component =
      match component.node with
      | Ast.ComponentDecl { cname; uses; provides; definitions = _ } ->
          let links = Hashtbl.create 0 in
          let value = ComponentData { uses; provides; links } in
          Hashtbl.add components_table cname value
    in

    let components_table = Hashtbl.create (List.length components) in
    List.iter (visit_component components_table) components;
    components_table

  let visit_links components links =
    let components_table = visit_components components in
    List.iter (visit_link components_table) links;
    check_used_interfaces_are_provided components components_table;
    components_table
end

module Qualifier = struct
  let rec visit_lvalue current_cname table lvalue =
    match lvalue.node with
    | Ast.AccVar (None, _) -> lvalue
    | Ast.AccIndex (l, e) ->
        let vl = visit_lvalue current_cname table l in
        let ve = visit_expr current_cname table e in
        Ast.AccIndex (vl, ve) ++ lvalue.annot
    | Ast.AccVar (Some interface, id) ->
        (* Add qualifier *)
        print_endline ("Accessing interface: " ^ interface ^ " in the table for identifier " ^id);
        let component =
          if interface = "Prelude" then "Prelude"
          else if interface = current_cname then current_cname
          else
            try 
            Hashtbl.find table interface
          with Not_found -> 
            let err_msg =
              Printf.sprintf
                "The interface '%s' is not provided by any component."
                interface
            in
            let help_msg =
              Printf.sprintf
                "In connect add a new link '%s.%s <- %s.%s'" current_cname
                interface "<ProvidingComponent>" interface
            in
            linking_error err_msg help_msg
        in

        Ast.AccVar (Some component, id) ++ lvalue.annot

  and visit_expr current_cname table expr =
    let new_node =
      match expr.node with
      | Ast.LV lvalue ->
          let vl = visit_lvalue current_cname table lvalue in
          Ast.LV vl
      | Ast.Assign (lvalue, expr) ->
          let vl = visit_lvalue current_cname table lvalue in
          let ve = visit_expr current_cname table expr in
          Ast.Assign (vl, ve)
      | Ast.AssignBinOp (lvalue, binop, expr) ->
          let vl = visit_lvalue current_cname table lvalue in
          let ve = visit_expr current_cname table expr in
          Ast.AssignBinOp (vl, binop, ve)
      | Ast.ILiteral _ -> expr.node
      | Ast.FLiteral _ -> expr.node
      | Ast.CLiteral _ -> expr.node
      | Ast.BLiteral _ -> expr.node
      | Ast.UnaryOp (uop, e) ->
          let ve = visit_expr current_cname table e in
          Ast.UnaryOp (uop, ve)
      | Ast.Address lvalue ->
          let vl = visit_lvalue current_cname table lvalue in
          Ast.Address vl
      | Ast.BinaryOp (binop, e1, e2) ->
          let ve1 = visit_expr current_cname table e1 in
          let ve2 = visit_expr current_cname table e2 in
          Ast.BinaryOp (binop, ve1, ve2)
      | Ast.Call (None, i2, exprs) ->
          let ve = List.map (visit_expr current_cname table) exprs in
          Ast.Call (Some(current_cname), i2, ve)
      | Ast.Call (Some interface, i2, exprs) ->
          (* Add qualifier *)
          print_endline ("Calling interface: " ^ interface ^ " in the table");
          let component =
            if interface = "Prelude" then "Prelude"
            else if interface = current_cname then current_cname
            else
              Hashtbl.find table interface
          in
          let ve = List.map (visit_expr current_cname table) exprs in
          Ast.Call (Some component, i2, ve)
      | Ast.IncDec (lvalue, inc_dec, pre_post) ->
          let vl = visit_lvalue current_cname table lvalue in
          Ast.IncDec (vl, inc_dec, pre_post)
    in
    new_node ++ expr.annot

  let rec visit_stmt current_cname table stmt =
    let node =
      match stmt.node with
      | Ast.If (e, s1, s2) ->
          let ve = visit_expr current_cname table e in
          let vs1 = visit_stmt current_cname table s1 in
          let vs2 = visit_stmt current_cname table s2 in
          Ast.If (ve, vs1, vs2)
      | Ast.While (e, s1) ->
          let ve = visit_expr current_cname table e in
          let vs1 = visit_stmt current_cname table s1 in
          Ast.While (ve, vs1)
      | Ast.DoWhile (e, s) ->
          let ve = visit_expr current_cname table e in
          let vs = visit_stmt current_cname table s in
          Ast.DoWhile (ve, vs)
      | Ast.For (e1, e2, e3, s) ->
          let ve1 =
            match e1 with None -> None | Some e -> Some (visit_expr current_cname table e)
          in
          let ve2 =
            match e2 with None -> None | Some e -> Some (visit_expr current_cname table e)
          in
          let ve3 =
            match e3 with None -> None | Some e -> Some (visit_expr current_cname table e)
          in
          let vs = visit_stmt current_cname table s in
          Ast.For (ve1, ve2, ve3, vs)
      | Ast.Expr e ->
          let ve = visit_expr current_cname table e in
          Ast.Expr ve
      | Ast.Return None -> Ast.Return None
      | Ast.Return (Some e) ->
          let ve = visit_expr current_cname table e in
          Ast.Return (Some ve)
      | Ast.Block stmtordec ->
          (* For each stmtordec in the list stmordec, create a new block in the symboltable *)
          let vstmtordec = List.map (visit_stmtordec current_cname table) stmtordec in
          Ast.Block vstmtordec
      | Ast.Skip -> Ast.Skip
    in
    node ++ stmt.annot

  and visit_stmtordec current_cname table stmtordec =
    match stmtordec.node with
    | Ast.LocalDecl _ -> stmtordec
    | Ast.Stmt s -> Ast.Stmt (visit_stmt current_cname table s) ++ stmtordec.annot

  let visit_definitions current_cname table definitions =
    let visit_definition definition =
      match definition.node with
      | Ast.FunDecl { rtype; fname; formals; body = Some stmt } ->
          let new_stmt = visit_stmt current_cname table stmt in
          Ast.FunDecl { rtype; fname; formals; body = Some new_stmt }
          ++ definition.annot
      | _ -> definition
    in
    List.map visit_definition definitions

  let visit_components table components =
    let visit_component component =
      match component.node with
      | Ast.ComponentDecl { cname; uses = _; provides = _; definitions } -> (
          match Hashtbl.find table cname with
          | ComponentData { uses; provides; links = links_table } ->
              let vdefinitions = visit_definitions cname links_table definitions in
              Ast.ComponentDecl
                { cname; uses; provides; definitions = vdefinitions }
              ++ component.annot)
    in
    List.map visit_component components
end

let wire_components ast =
  let it, ct, connections =
    match ast with
    | Ast.CompilationUnit { interfaces = i; components = co; connections = cn }
      ->
        (i, co, cn)
  in

  (* Check the semantic of the connections. *)
  let components_table = Checker.visit_links ct connections in

  (* print table *)
  print_endline "Components table:";
  Hashtbl.iter
    (fun k _ ->
      print_endline (k ^ " -> " ^ ("string_of_component_data")))
    components_table;

  (* Qualify the VarAcc and function Call by substituting the interface name with a concrete component. *)
  let qualified_components = Qualifier.visit_components components_table ct in

  (* Return the qualified AST. *)
  Ast.CompilationUnit
    { interfaces = it; components = qualified_components; connections }

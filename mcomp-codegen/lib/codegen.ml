open Ast
module L = Llvm

let ignore_case number message =
  failwith (Printf.sprintf "Error %d. This should not happen. " number ^ message)

let ignore_err number = ignore_case number ""
let debug_mode = true

(* TODO: should i have an exception? *)

type global_symbol_table = {
  functions : L.llvalue Symbol_table.t; (* Functions table *)
  variables : L.llvalue Symbol_table.t; (* Variables table *)
}


(* TODO re-organize *)
type global_context = {
  ll_module : L.llmodule;       (* LLVM module *)
  table : global_symbol_table;  (* Global symbols table *)
  cname : string option;        (* Name of the class *)
}

type fun_context = {
  ll_builder : L.llbuilder;         (* LLVM builder *)
  ll_value : L.llvalue;             (* LLVM function *)
  table : L.llvalue Symbol_table.t; (* Variables table *)
  global_ctx : global_context;      (* Global context *)
  return_type : Ast.typ;            (* Return type *)
}

let ll_ctx = L.global_context ()

(* Get references to the LLVM types *)
let int_type = L.i32_type ll_ctx
let bool_type = L.i1_type ll_ctx
let char_type = L.i8_type ll_ctx
let void_type = L.void_type ll_ctx


(* LLVM constants *)
let llvm_zero = L.const_int int_type 0

let rec ast_to_llvm = function
  | Ast.TInt  -> int_type
  | Ast.TBool -> bool_type
  | Ast.TChar -> char_type
  | Ast.TVoid -> void_type
  | Ast.TArray (typ, Some n)  -> L.array_type (ast_to_llvm typ) n
  | Ast.TArray (typ, None)
  | Ast.TRef typ -> L.pointer_type (ast_to_llvm typ)
  | Ast.TFun (params_types, rtype) ->
      let ll_rtype = ast_to_llvm rtype in
      let ll_params_types =
        params_types |> List.map ast_to_llvm |> Array.of_list
      in
      L.function_type ll_rtype ll_params_types
  | _ -> ignore_case 1 "ast_to_llvm: type not supported"

let var_init typ = L.undef (ast_to_llvm typ)

(* https://en.wikipedia.org/wiki/Name_mangling *)
let manglify component identifier = 
  let component = String.lowercase_ascii component in
  let identifier = String.lowercase_ascii identifier in
  "_" ^ component ^ "_" ^ identifier


let add_ending_instruction builder terminator = 
  (* If a terminator instruction is already present, do nothing, otherwise add the terminator *)
  let existing_terminator = L.block_terminator (L.insertion_block builder) in
  match existing_terminator with 
  | Some(instr) -> 
    (* if L.instr_opcode instr = L.Opcode.Ret then ()
    else ignore (terminator builder) *)
    ()
  | None -> ignore (terminator builder)


let build_alloca typ id builder =  
  match typ with
  | Ast.TArray (_, Some n) -> L.build_array_alloca (ast_to_llvm typ) (L.const_int int_type n) id builder
  | _ -> L.build_alloca (ast_to_llvm typ) id builder  

let gen_unary_op uop typ e ll_builder = 
  let fun_build = match (uop, typ) with
  | (Ast.Neg, Ast.TInt)   -> L.build_neg e "t_neg"
  | (Ast.Not, Ast.TBool)  -> L.build_not e "t_not"
  | _ -> ignore_case 2 "gen_unary_op: type not supported"
  in fun_build ll_builder

(* CODEGEN *)
  let rec gen_binary_op binop typ1 typ2 e1_generator e2_generator fun_ctx = 
    let aux_non_boolean binop fun_ctx =
      let gen_e1 = e1_generator fun_ctx in
      let gen_e2 = e2_generator fun_ctx in
      match binop with
      (* Math operators *)
      | Ast.Add     ->    L.build_add   gen_e1 gen_e2 "t_add" 
      | Ast.Sub     ->    L.build_sub   gen_e1 gen_e2 "t_sub"
      | Ast.Mult    ->    L.build_mul   gen_e1 gen_e2 "t_mult"
      | Ast.Div     ->    L.build_udiv  gen_e1 gen_e2 "t_div"
      | Ast.Mod     ->    L.build_srem  gen_e1 gen_e2 "t_mod"
  
      (* Compare operators *)
      | Ast.Equal   ->    L.build_icmp L.Icmp.Eq  gen_e1 gen_e2 "t_eq"
      | Ast.Neq     ->    L.build_icmp L.Icmp.Ne  gen_e1 gen_e2 "t_neq"
      | Ast.Less    ->    L.build_icmp L.Icmp.Slt gen_e1 gen_e2 "t_lt"
      | Ast.Leq     ->    L.build_icmp L.Icmp.Sle gen_e1 gen_e2 "t_leq"
      | Ast.Greater ->    L.build_icmp L.Icmp.Sgt gen_e1 gen_e2 "t_gt"
      | Ast.Geq     ->    L.build_icmp L.Icmp.Sge gen_e1 gen_e2 "t_geq"
      | _ -> ignore_case 3 "gen_binary_op: boolean operators are handled elsewhere"
    in 
    
    let fun_build = match binop with
    (* Boolean operators needs a lazy codegen of the operands in the proper blocks *)
    | Ast.And     ->    gen_and_short_circuit fun_ctx e1_generator e2_generator 
    | Ast.Or      ->    gen_or_short_circuit fun_ctx e1_generator e2_generator 
    (* Other operators *)
    | _ -> aux_non_boolean binop fun_ctx
    in fun_build fun_ctx.ll_builder


and gen_and_short_circuit fun_ctx left_e_generator right_e_generator = 
  let continue_block = L.append_block ll_ctx "and_sc_continue" fun_ctx.ll_value in
  let end_block = L.append_block ll_ctx "and_sc_end" fun_ctx.ll_value in
  
  let g_left_e = left_e_generator fun_ctx in
  let incoming_bb_1 = L.insertion_block fun_ctx.ll_builder in
  (* If the left expression is true continue, otherwise jump to the end block *)
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_cond_br g_left_e continue_block end_block) in
  L.position_at_end continue_block fun_ctx.ll_builder;
  
  let g_right_e = right_e_generator fun_ctx in
  let incoming_bb_2 = L.insertion_block fun_ctx.ll_builder in
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br end_block) in
  L.position_at_end end_block fun_ctx.ll_builder;

  L.build_phi [(g_left_e, incoming_bb_1); (g_right_e, incoming_bb_2)] "t_phi_and_sc"

and gen_or_short_circuit fun_ctx left_e_generator right_e_generator = 
  let continue_block = L.append_block ll_ctx "or_sc_continue" fun_ctx.ll_value in
  let end_block = L.append_block ll_ctx "or_sc_end" fun_ctx.ll_value in
  
  let g_left_e = left_e_generator fun_ctx in
  let incoming_bb_1 = L.insertion_block fun_ctx.ll_builder in
  (* If the left expression is true jump to the end block, otherwise continue *)
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_cond_br g_left_e end_block continue_block) in
  L.position_at_end continue_block fun_ctx.ll_builder;
  
  let g_right_e = right_e_generator fun_ctx in
  let incoming_bb_2 = L.insertion_block fun_ctx.ll_builder in
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br end_block) in
  L.position_at_end end_block fun_ctx.ll_builder;

  L.build_phi [(g_left_e, incoming_bb_1); (g_right_e, incoming_bb_2)] "t_phi_or_sc"

(* and gen_and_short_circuit fun_ctx left_e right_e = 
  let continue_block = L.append_block ll_ctx "and_sc_continue" fun_ctx.ll_value in
  let end_block = L.append_block ll_ctx "and_sc_end" fun_ctx.ll_value in
  
  let g_left_e = gen_expr fun_ctx left_e in
  let incoming_bb_1 = L.insertion_block fun_ctx.ll_builder in
  (* If the left expression is true continue, otherwise jump to the end block *)
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_cond_br g_left_e continue_block end_block) in
  L.position_at_end continue_block fun_ctx.ll_builder;
  
  let g_right_e = gen_expr fun_ctx right_e in
  let incoming_bb_2 = L.insertion_block fun_ctx.ll_builder in
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br end_block) in
  L.position_at_end end_block fun_ctx.ll_builder;

  L.build_phi [(g_left_e, incoming_bb_1); (g_right_e, incoming_bb_2)] "t_phi_and_sc"

and gen_or_short_circuit fun_ctx left_e right_e = 
  let continue_block = L.append_block ll_ctx "or_sc_continue" fun_ctx.ll_value in
  let end_block = L.append_block ll_ctx "or_sc_end" fun_ctx.ll_value in
  
  let g_left_e = gen_expr fun_ctx left_e in
  let incoming_bb_1 = L.insertion_block fun_ctx.ll_builder in
  (* If the left expression is true jump to the end block, otherwise continue *)
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_cond_br g_left_e end_block continue_block) in
  L.position_at_end continue_block fun_ctx.ll_builder;
  
  let g_right_e = gen_expr fun_ctx right_e in
  let incoming_bb_2 = L.insertion_block fun_ctx.ll_builder in
  let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br end_block) in
  L.position_at_end end_block fun_ctx.ll_builder;

  L.build_phi [(g_left_e, incoming_bb_1); (g_right_e, incoming_bb_2)] "t_phi_or_sc"

and gen_expr fun_ctx expr =
  match expr.node with
  | Ast.LV(lvalue) ->
    gen_lvalue ~address:false ~load_value:true fun_ctx lvalue
    
  | Ast.Assign(lvalue, expr) ->
    let g_expr = gen_expr fun_ctx expr in
    let g_lvalue = match expr.node with 
    | Ast.Address(_) -> 
      (* Get the address of the lvalue *)
      gen_lvalue ~address:true ~load_value:false fun_ctx lvalue
    | _ -> 
      (* Otherwise it, load it before *)
      gen_lvalue ~address:false ~load_value:false fun_ctx lvalue
    in
    L.build_store g_expr g_lvalue fun_ctx.ll_builder |> ignore;
    g_expr

  | Ast.AssignBinOp(lvalue, binop, expr) ->
    let g_lvalue = gen_lvalue ~address:false ~load_value:false fun_ctx lvalue in
    let lvalue_generator = fun fun_ctx -> L.build_load g_lvalue "t_load" fun_ctx.ll_builder in
    let expr_generator = fun fun_ctx -> gen_expr fun_ctx expr in
    (* By using the closure, here the lvalue is generated only once *)
    let g_binop = gen_binary_op binop lvalue.annot expr.annot lvalue_generator expr_generator fun_ctx in
    L.build_store g_binop g_lvalue fun_ctx.ll_builder |> ignore;
    g_binop

  | Ast.ILiteral(i) -> L.const_int int_type i
  | Ast.CLiteral(c) -> L.const_int char_type (Char.code c)
  | Ast.BLiteral(b) -> L.const_int bool_type (if b then 1 else 0)

  | Ast.UnaryOp(uop, e) -> 
    let g_e = gen_expr fun_ctx e in
    gen_unary_op uop e.annot g_e fun_ctx.ll_builder

  | Ast.Address(lvalue) -> 
    (* Get the address of the lvalue *)
    gen_lvalue ~address:true ~load_value:false fun_ctx lvalue

  | Ast.BinaryOp(binop, e1, e2) ->
    gen_binary_op binop e1.annot e2.annot e1 e2 fun_ctx

  | Ast.Call(Some(cname), fun_name, exprs) ->
    begin
    print_endline ("call: " ^ cname ^ "." ^ fun_name);
    let mangled_name = if fun_name = "main" then fun_name else manglify cname fun_name in
    let ll_fun = Symbol_table.lookup mangled_name fun_ctx.global_ctx.table.functions in
    let g_exprs = Array.of_list (List.map (gen_expr fun_ctx) exprs) in
    match expr.annot with 
    | Ast.TVoid -> L.build_call ll_fun g_exprs "" fun_ctx.ll_builder
    | _ -> L.build_call ll_fun g_exprs "t_call" fun_ctx.ll_builder
    end
  | Ast.Call(None, _, _) -> ignore_case 5 "gen_expr: Call: None"

  | Ast.IncDec(lvalue, inc_dec, pre_post) ->
     (* Get the lvalue *)
    let g_lvalue = gen_lvalue ~address:false ~load_value:false fun_ctx lvalue in
    let g_lvalue_value = L.build_load g_lvalue "t_load" fun_ctx.ll_builder in
    let one = L.const_int int_type 1 in
    (* Apply increment or decrement to the value *)
    let g_result = match inc_dec with
    | Ast.Inc -> L.build_add g_lvalue_value one "t_inc" fun_ctx.ll_builder
    | Ast.Dec -> L.build_sub g_lvalue_value one "t_dec" fun_ctx.ll_builder
    in
    (* Store the new value *)
    let _ = L.build_store g_result g_lvalue fun_ctx.ll_builder in
    (* Return the new result (e.g. pre) or the old value (e.g. post) *)
    match pre_post with
    | Ast.Pre -> g_result
    | Ast.Post -> g_lvalue_value

(* Currently it returns an address *)
and gen_lvalue ?(address=false) ?(load_value=false) ctx lvalue =
  let lookup cname id = match cname with
  | Some(cname) -> 
    (* Look up the mangled name in the global symbol table *)
    let mangled_name = manglify cname id in
    Symbol_table.lookup mangled_name ctx.global_ctx.table.variables
  | None -> 
    (* Look up the unmangled name in the current function's symbol table *)
    begin
    try 
      Symbol_table.lookup id ctx.table
    with Symbol_table.NotFound(_) -> 
      (* TODO: i don't know why I here there coul be the case where the is no cname for a global variable *)
      (* If not found, look up the mangled name in the global symbol table *)
      let cname = match ctx.global_ctx.cname with | Some(cname) -> cname | None -> "" in
      let mangled_name = manglify cname id in 
      Symbol_table.lookup mangled_name ctx.global_ctx.table.variables
    end
    in

  match lvalue.node with
  | Ast.AccVar(cname, id) -> 
    print_endline ("gen_lvalue: AccVar: " ^ id);
    let var_sym = lookup cname id in
    (* Determine what to do based on the lvalue's type annotation *)
    begin
    match lvalue.annot with
    | Ast.TRef(_) -> 
      (* If we need just the address of the reference, return it directly *)
      if address then
        begin
          Printf.printf "gen_lvalue: AccVar: address of reference\n";
          var_sym
        end
      else
        (* Otherwise, load the address of the reference from memory *)
        let address = L.build_load var_sym "t_lv_address" ctx.ll_builder in
        if load_value then
          (* If we also need the value that the reference points to, load it from memory *)
          L.build_load address "t_lv_value" ctx.ll_builder
        else
          (* Otherwise, just return the address of the reference *)
          address

    | Ast.TArray(_, Some _) -> 
      Llvm.build_in_bounds_gep var_sym [| llvm_zero; llvm_zero |] "t_index_address" ctx.ll_builder

    (* If the lvalue is a primitive type (not a reference) *)
    | _ -> 
      (* If we need the value stored in this variable, load it from memory *)
      if load_value then 
        L.build_load var_sym "t_lv_value" ctx.ll_builder
      else 
        (* Otherwise, just return the address of the variable *)
        begin
          Printf.printf "gen_lvalue: AccVar: address of variable\n";
          var_sym
        end
    end
  
  | Ast.AccIndex(lvalue, expr) -> 
    let var_sym = match lvalue.node with | Ast.AccVar(cname, id) -> lookup cname id | _ -> ignore_case 3 "gen_lvalue: AccIndex: not AccVar" in
    let g_index = gen_expr ctx expr in
    match lvalue.annot with
    (* Arrays with a fixed size needs to be dereferences with a zero-index before using the next index to get the pointer *)
    | Ast.TArray(typ, Some(n)) -> 
      let address = L.build_in_bounds_gep var_sym [| llvm_zero; g_index |] "t_index_address_bound" ctx.ll_builder in
      if load_value then
        match typ with
        | Ast.TRef(_) -> 
          let value_address = L.build_load address "t_index_value" ctx.ll_builder in 
          L.build_load value_address "t_index_value" ctx.ll_builder
        | _ -> L.build_load address "t_index_value" ctx.ll_builder
      else
        address
    (* Arrays without are dereferenced like pointer to a contiguos memory area. *)
    | Ast.TRef(_)
    | Ast.TArray(_, None) ->
      (* TODO: check this is new *)
      let x = L.build_load var_sym "t_index_address_unbound" ctx.ll_builder in
      let address = L.build_in_bounds_gep x [| g_index |] "t_index_address_unbound" ctx.ll_builder in
      if load_value then
        L.build_load address "t_index_value" ctx.ll_builder
      else
        address
    | _ -> ignore_case 6 "gen_lvalue: AccIndex: type not supported\n"

let rec gen_stmt fun_ctx stmt = 
  match stmt.node with
  | Ast.If(e, s1, s2) ->
    (* Create the necessary basic blocks *)
    let then_block  = L.append_block ll_ctx "then"  fun_ctx.ll_value in
    let else_block  = L.append_block ll_ctx "else"  fun_ctx.ll_value in
    let merge_block = L.append_block ll_ctx "merge" fun_ctx.ll_value in

    (* Generate code for the condition expression *)
    let g_e = gen_expr fun_ctx e in

    (* Create a conditional branch based on the value of the condition *)
    let _ = L.build_cond_br g_e then_block else_block fun_ctx.ll_builder in

    (* Move the builder to the end of the then block and generate code for the then branch *)
    let _ = L.position_at_end then_block fun_ctx.ll_builder in
    let g_s1 = gen_stmt fun_ctx s1 in
    (* Create an unconditional branch from the end of the then block to the merge block *)
    (* let _ = L.build_br merge_block fun_ctx.ll_builder in TODO: remove *)
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br merge_block) in

    (* Move the builder to the end of the else block and generate code for the else branch *)
    let _ = L.position_at_end else_block fun_ctx.ll_builder in
    let g_s2 = gen_stmt fun_ctx s2 in
    (* Create an unconditional branch from the end of the else block to the merge block *)
    (* let _ = L.build_br merge_block fun_ctx.ll_builder in TODO: remove*) 
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br merge_block) in 

    (* Move the builder to the end of the merge block and create a phi instruction to choose the correct value based on the control flow *)
    let _ = L.position_at_end merge_block fun_ctx.ll_builder in
    (* let phi = L.build_phi [(v1, then_block); (v2, else_block)] "if_result" fun_ctx.ll_builder in *)
    (* ignore phi; *)
    true
  
  | Ast.While(e, s1) ->
    (* Create the necessary basic blocks for the loop *)
    let condition = L.append_block ll_ctx "while_condition" fun_ctx.ll_value in
    let body = L.append_block ll_ctx "while_body" fun_ctx.ll_value in
    let continue = L.append_block ll_ctx "while_continue" fun_ctx.ll_value in

    (* Transfer control to the condition block *)
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br condition) in
    (* Set the builder position to the start of the condition block *)
    let _ = L.position_at_end condition fun_ctx.ll_builder in
    (* Generate code for the loop condition that checks whether to continue the loop or exit *)
    let g_e = gen_expr fun_ctx e in
    (* let _ = L.build_cond_br g_e body continue fun_ctx.ll_builder in TODO: remove *)
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_cond_br g_e body continue) in


    (* Set the builder position to the start of the body block *)
    let _ = L.position_at_end body fun_ctx.ll_builder in
    (* Generate code for the body of the loop *)
    let _ = gen_stmt fun_ctx s1 in
    (* Transfer control back to the condition block *)
    (* let _ = L.build_br condition fun_ctx.ll_builder in TODO: remove *)
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br condition) in

    (* Set the builder position to the start of the continue block *)
    let _ = L.position_at_end continue fun_ctx.ll_builder in

    true
  | Ast.DoWhile(e, s) -> 
    true


  | Ast.For(e1, e2, e3, s) -> 
    (* Create the necessary basic blocks for the loop *)
    let condition = L.append_block ll_ctx "for_condition" fun_ctx.ll_value in
    let body = L.append_block ll_ctx "for_body" fun_ctx.ll_value in
    let continue = L.append_block ll_ctx "for_continue" fun_ctx.ll_value in

    (* Generate code for the initialization expression *)
    let g_e1 = match e1 with 
    | Some(e) -> gen_expr fun_ctx e
    | None ->  L.const_int int_type 0
    in

    (* Transfer control to the condition block *)
    (* let _ = L.build_br condition fun_ctx.ll_builder in *)
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br condition) in
    (* Set the builder position to the start of the condition block *)
    let _ = L.position_at_end condition fun_ctx.ll_builder in
    (* Generate code for the loop condition that checks whether to continue the loop or exit *)
    let g_e2 = match e2 with
    | Some(e) -> gen_expr fun_ctx e
    | None -> L.const_int int_type 1
    in
    (* let _ = L.build_cond_br g_e2 body continue fun_ctx.ll_builder in *)
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_cond_br g_e2 body continue) in


    (* Set the builder position to the start of the body block *)
    let _ = L.position_at_end body fun_ctx.ll_builder in
    (* Generate code for the body of the loop *)
    let _ = gen_stmt fun_ctx s in
    (* Generate code for the increment expression *)
    let g_e3 = match e3 with
    | Some(e) -> gen_expr fun_ctx e
    | None -> L.const_int int_type 0
    in
    (* Transfer control back to the condition block *)
    (* let _ = L.build_br condition fun_ctx.ll_builder in *)
    let _ = add_ending_instruction fun_ctx.ll_builder (L.build_br condition) in

    (* Set the builder position to the start of the continue block *)
    let _ = L.position_at_end continue fun_ctx.ll_builder in


    true

  | Ast.Expr(e) -> let _ = gen_expr fun_ctx e in (); true

  | Ast.Return(None) ->
    add_ending_instruction fun_ctx.ll_builder (L.build_ret_void); false
  
  | Ast.Return(Some(e)) ->
    add_ending_instruction fun_ctx.ll_builder (L.build_ret (gen_expr fun_ctx e)); false
  | Ast.Block(stmtordec) -> 
    (* TODO: be aware of this, because in fundecl I already create a new block *)
    let fun_ctx = {fun_ctx with table = Symbol_table.begin_block fun_ctx.table} in
    let continue stmtordec = gen_stmtordec fun_ctx stmtordec in
    List.for_all continue stmtordec
    (* List.iter (gen_stmtordec fun_ctx) stmtordec; *)
    (* let fun_ctx = {fun_ctx with table = Symbol_table.end_block fun_ctx.table} in *)
    (* begin
      (* match the return type of the function and based on that decide what to return if there is no explicit return statement *)
      match fun_ctx.return_type with 
      | Ast.TVoid -> add_ending_instruction fun_ctx.ll_builder (L.build_ret_void); false
      | _ -> add_ending_instruction fun_ctx.ll_builder (L.build_ret (L.const_int int_type 0)); false
    end *)
  | Ast.Skip -> false

and gen_stmtordec fun_ctx stmtordec =
  match stmtordec.node with 
  | Ast.Stmt(stmt) ->  gen_stmt fun_ctx stmt
  | Ast.LocalDecl((id, typ)) -> 
    (* TODO: check weather it's an array with a fixed size *)
    (* Define a local variable *)
    let local_var = build_alloca typ id fun_ctx.ll_builder in
    (* initialize it *)
    
    (* Keep track of the local variable in the symbol table *)
    Symbol_table.add_entry id local_var fun_ctx.table |> ignore;
    true
    


let gen_local_args (fun_ctx : fun_context) (id, typ) argument = 
    (* TODO: change the identifier with a proper name in order to lookup it  *)
    let local_arg = build_alloca typ id fun_ctx.ll_builder in
    print_endline ("Just build local arg: " ^ id);
    (* Store the reference in the symbol table TODO:??? *)
    let _ = Symbol_table.add_entry id local_arg fun_ctx.table in
    (* print table *)
    print_endline (Symbol_table.show fun_ctx.table);
    (* Load the function argument in the local variable *)
    let _ = L.build_store argument local_arg fun_ctx.ll_builder in
    ()

let gen_fundecl (ctx : global_context) f =
  (* TODO: there should be a function to lookup previous functions declarations and change them, check it out *)
  let ll_fun = Symbol_table.lookup f.fname ctx.table.functions in
  let ll_builder = L.builder_at_end ll_ctx (L.entry_block ll_fun) in
  let fun_table = Symbol_table.begin_block ctx.table.variables in
  let fun_ctx = {ll_builder = ll_builder; ll_value = ll_fun; table = fun_table; global_ctx = ctx; return_type = f.rtype} in
  (* Allocate the function's arguments into the stack *)
  let ll_params = Array.to_list (L.params ll_fun) in
  let _ = List.iter2 (gen_local_args fun_ctx) f.formals ll_params in
  let _ = match f.body with | Some(body) -> gen_stmt fun_ctx body | None -> ignore_err 1 in
  (* Add return type *)
  match f.rtype with
  | Ast.TVoid -> add_ending_instruction ll_builder (L.build_ret_void)
  | _ -> 
    (* get default value for a specific type *)
    let default_value = var_init f.rtype in
    add_ending_instruction ll_builder (L.build_ret default_value);
  (* Check all the basic blocks and remove the one without predecssors *)

  (* let _ = L.build_ret_void ll_builder in TODO: ret type is not the correct one *)
  ()

let gen_vardecl ctx (id, typ) =
  let init = var_init typ in
  let var = L.define_global id init ctx.ll_module in
  let _ = Symbol_table.add_entry id var ctx.table.variables in ()

let gen_definition ctx definition =
match definition.node with
| Ast.FunDecl(f) -> (gen_fundecl ctx f)
| _ -> ()

let gen_component ctx component =
  match component.node with
  | Ast.ComponentDecl({cname; uses=_; provides=_; definitions}) ->
    (* change component_name inside ctx *)
    (* TODO: is this used somewhere? WTF *)
    let ctx = {ctx with cname = Some(cname)} in
    (* ctx.component <- Some(cname); *)
    (* create a new symbol table for the component *)
    Printf.printf "\nCODEGEN component %s\n" cname;
    List.iter (gen_definition ctx) definitions;
    ()





    

(* DECLARATION *)


let declare_member ctx cname member =
  let node = match member.node with
  | Ast.FunDecl f ->
      let ll_typ = ast_to_llvm member.annot in
      let mangled_name = if f.fname = "main" then f.fname else manglify cname f.fname in
      let ll_fun = L.define_function mangled_name ll_typ ctx.ll_module in
      Symbol_table.add_entry mangled_name ll_fun ctx.table.functions |> ignore;

      Ast.FunDecl {f with fname = mangled_name}

  | Ast.VarDecl (id, typ) ->
      let mangled_name = manglify cname id in
      let ll_init = var_init typ in
      let ll_var = L.define_global mangled_name ll_init ctx.ll_module in
      Symbol_table.add_entry mangled_name ll_var ctx.table.variables |> ignore;

      Ast.VarDecl (mangled_name, typ)
  in {member with node}

let declare_component ctx component =
  let node = match component.node with
  | Ast.ComponentDecl { cname; uses; provides; definitions } ->
      Printf.printf "\nDECLARE component %s\n" cname;
      (* TODO: should I add the component to the table? *)
      let definitions = List.map (declare_member ctx cname) definitions in
      Ast.ComponentDecl { cname; uses; provides; definitions }
  in
  { component with node }
  
(* This defines a symbol with the original name (e.g. print) but the symbol table will have prelude_print*)
let declare_function ctx cname (fname, ftyp) =
  let mangled_name = manglify cname fname in
  let ll_fun = L.declare_function mangled_name (ast_to_llvm ftyp) ctx.ll_module in
  Symbol_table.add_entry mangled_name ll_fun ctx.table.functions |> ignore

let to_llvm_module ast =
  print_endline "\n====================VISIT====================";

  let components =
    match ast with
    | Ast.CompilationUnit { interfaces = _; components = co; _ } -> co
  in
  let llmodule = L.create_module ll_ctx "mcomp_global" in
  let global_table =
    {
      functions = Symbol_table.empty_table ();  
      variables = Symbol_table.empty_table ();
    }
  in
  let fun_table = Symbol_table.empty_table () in
  let ctx = { ll_module = llmodule; table = global_table; cname = None;} in

  let _ =
    List.iter (declare_function ctx "prelude") Mcomp_stdlib.prelude_signature
  in
  (* Declare the members (e.g. global variables and functions) *)
  (* TODO: figure out why this step is needed before the codegen?
     Why do I need all the members initially loaded in the namespace from the beginning? *)
  (* Returns the components with the members' identifiers mangled with the component name
     This allows to declare the global variables and the component functions
      already with the correct name for the next phase  *)
  let components = List.map (declare_component ctx) components in
  (* Codegen *)
  let _ = List.iter (gen_component ctx) components in

  (* let _ = (declare_prelude_functions) llmodule in *)
  (* List.iter (declare_components ctx) components; *)
  (* L.dump_module llmodule; *)
  (* debug *)
  if debug_mode then
    begin
    print_endline "\n====================DEBUG====================";
    print_endline "Symbol table functions:";  
    let s = Symbol_table.show ctx.table.functions in 
    print_endline s;
    print_endline "Symbol table variables:";
    let s = Symbol_table.show ctx.table.variables in
    print_endline s;
    print_endline "\n====================LLVM====================";
    end
  else ();

  llmodule

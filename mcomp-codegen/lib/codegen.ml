(** This module is the implementation of the code generation that uses the ocaml binding,
for Ocaml. It takes an annotated AST with types and produces an LLVM module. *)
open Ast
module L = Llvm

(** A flag to enable the printing of the main symbol tables *)
let debug_mode = true

let ignore_case number message =
  failwith (Printf.sprintf "Error %d. This should not happen. " number ^ message)

let ignore_err number = ignore_case number ""

(** LLVM global context *)
let ll_ctx = L.global_context ()

(** The global symbol table to keep track of the global namespace for variables and functions *)
type global_symbol_table = {
  functions : L.llvalue Symbol_table.t;   (* Functions table *)
  variables : L.llvalue Symbol_table.t;   (* Global variables table *)
}

(** An helper global context that is passed around for the codegen *)
type global_context = {
  ll_module : L.llmodule;                 (* LLVM module *)
  table : global_symbol_table;            (* Global symbols table *)
  cname : string option;                  (* Name of the component *)
  global_ctors:  L.llvalue * L.llbuilder; (* Global constructors function used for global variable initialization *)
}

(** An helper context to keep track of information needed for the function codegen *)
type fun_context = {
  ll_builder : L.llbuilder;               (* LLVM builder *)
  ll_value : L.llvalue;                   (* LLVM function *)
  table : L.llvalue Symbol_table.t;       (* Local variables table *)
  global_ctx : global_context;            (* Global context *)
}     

(* Short-hand references to the LLVM types *)
let int_type    = L.i32_type ll_ctx
let float_type  = L.float_type ll_ctx
let bool_type   = L.i1_type ll_ctx
let char_type   = L.i8_type ll_ctx
let void_type   = L.void_type ll_ctx


(* Short-hand LLVM constants *)
let llvm_zero_i = L.const_int int_type 0
let llvm_one_i = L.const_int int_type 1
let llvm_one_f = L.const_float float_type 1.0


(** 
  Given an AST type, return the corresponding LLVM type.
  @param: typ The AST type
  @return: The corresponding LLVM type
*)
let rec ast_to_llvm = function
  | Ast.TInt  -> int_type
  | Ast.TFloat -> float_type
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


(**
  Given an AST type, return the undefined value of the corresponding LLVM type.
  @param: typ The AST type
  @return: The undefined value of the corresponding LLVM type
*)
let var_init typ = L.undef (ast_to_llvm typ)


(**
  Given a builder, it builds an alloca instruction for the given type and id. Used
  for local variables to allocate space in the stack.
  @param: builder The LLVM builder
  @param: typ The type of the variable
  @param: id The id of the variable
*)
let build_alloca typ id builder =  
  match typ with
  | Ast.TArray (_, Some n) -> L.build_array_alloca (ast_to_llvm typ) (L.const_int int_type n) id builder
  | _ -> L.build_alloca (ast_to_llvm typ) id builder  


(** 
  Given a builder, it adds a given terminator in the current block if it is not already present.
  @param: builder The LLVM builder
  @param: terminator The terminator to add
*)
let add_ending_instruction terminator builder = 
  (* If a terminator instruction is already present, do nothing, otherwise add the terminator *)
  let existing_terminator = L.block_terminator (L.insertion_block builder) in
  match existing_terminator with 
  | Some(_) -> ()
  | None -> ignore (terminator builder)



(* CODEGEN SUB-MODULE*)
module Codegen = struct    

  (** 
    Given a builder it builds at the current position,
    a unary operation on the given expression based on the given type.
    @param: uop The unary operator
    @param: typ The type of the expression
    @param: e The expression
    @param: ll_builder The LLVM builder
    @return: The LLVM value of the virtual register containing the result of the operation
  *)
  let gen_unary_op uop typ e ll_builder = 
    let fun_build = match (uop, typ) with
      | (Ast.Neg, Ast.TInt)   -> L.build_neg e "neg"
      | (Ast.Neg, Ast.TFloat) -> L.build_fneg e "fneg"
      | (Ast.Not, Ast.TBool)  -> L.build_not e "not"
      | _ -> ignore_case 2 "gen_unary_op: type not supported"
    in fun_build ll_builder


  (**
    Given a builder it builds at the current position,
    a binary operation on the given expressions based on the given types.
    @param: binop The binary operator
    @param: typ1 The type of the first expression
    @param: typ2 The type of the second expression
    @param: e1 The first expression
    @param: e2 The second expression
    @param: ll_builder The LLVM builder
    @return: The LLVM value of the virtual register containing the result of the operation
  *)
  let rec gen_binary_op binop typ1 typ2 e1_generator e2_generator fun_ctx = 
    (* Auxiliary function used to build binary operators for non-boolean types *)
    let aux_non_boolean binop typ1 typ2 fun_ctx =
      let gen_e1 = e1_generator fun_ctx in
      let gen_e2 = e2_generator fun_ctx in
      match (typ1, typ2) with
      (* Match int or &int *)
      | _, _ when Ast.is_of_typ_or_reftyp Ast.TInt typ1 && Ast.is_of_typ_or_reftyp Ast.TInt typ2 ->
        begin
          match binop with
          (* Math operators *)
          | Ast.Add     ->    L.build_add   gen_e1 gen_e2 "add" 
          | Ast.Sub     ->    L.build_sub   gen_e1 gen_e2 "sub"
          | Ast.Mult    ->    L.build_mul   gen_e1 gen_e2 "mult"
          | Ast.Div     ->    L.build_udiv  gen_e1 gen_e2 "div"
          | Ast.Mod     ->    L.build_srem  gen_e1 gen_e2 "mod"
      
          (* Compare operators - Signed *)
          | Ast.Equal   ->    L.build_icmp L.Icmp.Eq  gen_e1 gen_e2 "eq"
          | Ast.Neq     ->    L.build_icmp L.Icmp.Ne  gen_e1 gen_e2 "neq"
          | Ast.Less    ->    L.build_icmp L.Icmp.Slt gen_e1 gen_e2 "lt"
          | Ast.Leq     ->    L.build_icmp L.Icmp.Sle gen_e1 gen_e2 "leq"
          | Ast.Greater ->    L.build_icmp L.Icmp.Sgt gen_e1 gen_e2 "gt"
          | Ast.Geq     ->    L.build_icmp L.Icmp.Sge gen_e1 gen_e2 "geq"

          | _ -> ignore_case 3 "gen_binary_op: boolean operators are handled elsewhere"
        end
      (* Match float or &float *)
      | _, _ when Ast.is_of_typ_or_reftyp Ast.TFloat typ1 && Ast.is_of_typ_or_reftyp Ast.TFloat typ2 ->
        begin
          match binop with 
          (* Float-Math operators *)
          | Ast.Add     ->    L.build_fadd  gen_e1 gen_e2 "fadd"
          | Ast.Sub     ->    L.build_fsub  gen_e1 gen_e2 "fsub"
          | Ast.Mult    ->    L.build_fmul  gen_e1 gen_e2 "fmult"
          | Ast.Div     ->    L.build_fdiv  gen_e1 gen_e2 "fdiv"
      
          (* Float-Compare operators *)
          (* Ordered that allows to be IEEE 754 compliant. e.g. NaN == NaN -> False *)
          | Ast.Equal   ->    L.build_fcmp L.Fcmp.Oeq gen_e1 gen_e2 "feq"
          | Ast.Neq     ->    L.build_fcmp L.Fcmp.One gen_e1 gen_e2 "fneq"
          | Ast.Less    ->    L.build_fcmp L.Fcmp.Olt gen_e1 gen_e2 "flt"
          | Ast.Leq     ->    L.build_fcmp L.Fcmp.Ole gen_e1 gen_e2 "fleq"
          | Ast.Greater ->    L.build_fcmp L.Fcmp.Ogt gen_e1 gen_e2 "fgt"
          | Ast.Geq     ->    L.build_fcmp L.Fcmp.Oge gen_e1 gen_e2 "fgeq"

          | _ -> ignore_case 3 "gen_binary_op: boolean operators are handled elsewhere"
        end
      | _ -> ignore_case 4 "gen_binary_op: type not supported"
    
    in

    let fun_build = match binop with
      (* Boolean operators needs a lazy codegen of the operands
          in the proper blocks to support short-circuiting *)
      | Ast.And     ->    gen_and_short_circuit fun_ctx e1_generator e2_generator 
      | Ast.Or      ->    gen_or_short_circuit fun_ctx e1_generator e2_generator 
      (* Non-boolean operators *)
      | _ -> aux_non_boolean binop typ1 typ2 fun_ctx
    
    in fun_build fun_ctx.ll_builder


  (**
    Generates the LLVM code for the short-circuiting of the AND operator
    @param: fun_ctx The context containing information about the current function and global state
    @param: left_e_generator The generator of the left expression
    @param: right_e_generator The generator of the right expression
    @return: The LLVM value of the virtual register containing the result of the operation
  *)
  and gen_and_short_circuit fun_ctx left_e_generator right_e_generator = 
    let continue_block = L.append_block ll_ctx "and_sc_continue" fun_ctx.ll_value in
    let end_block = L.append_block ll_ctx "and_sc_end" fun_ctx.ll_value in
    
    (* Codegen the left expression *)
    let g_left_e = left_e_generator fun_ctx in
    let incoming_bb_1 = L.insertion_block fun_ctx.ll_builder in
    (* If the left expression is true then evaluate the second expression, otherwise jump directly to the end *)
    let _ = add_ending_instruction (L.build_cond_br g_left_e continue_block end_block) fun_ctx.ll_builder in
    L.position_at_end continue_block fun_ctx.ll_builder;
    
    (* Codegen the right expression *)
    let g_right_e = right_e_generator fun_ctx in
    let incoming_bb_2 = L.insertion_block fun_ctx.ll_builder in
    (* Jump to the end block *)
    let _ = add_ending_instruction (L.build_br end_block) fun_ctx.ll_builder in
    L.position_at_end end_block fun_ctx.ll_builder;

    (* Create the phi node to get the final value of the AND operation *)
    L.build_phi [(g_left_e, incoming_bb_1); (g_right_e, incoming_bb_2)] "phi_and_sc"


  (**
    Generates the LLVM code for the short-circuiting of the OR operator
    @param: fun_ctx The context containing information about the current function and global state
    @param: left_e_generator The generator of the left expression
    @param: right_e_generator The generator of the right expression
    @return: The LLVM value of the virtual register containing the result of the operation
  *)
  and gen_or_short_circuit fun_ctx left_e_generator right_e_generator = 
    let continue_block = L.append_block ll_ctx "or_sc_continue" fun_ctx.ll_value in
    let end_block = L.append_block ll_ctx "or_sc_end" fun_ctx.ll_value in
    
    (* Codegen the left expression *)
    let g_left_e = left_e_generator fun_ctx in
    let incoming_bb_1 = L.insertion_block fun_ctx.ll_builder in
    (* If the left expression is false then evaluate the second expression, otherwise jump directly to the end *)
    let _ = add_ending_instruction (L.build_cond_br g_left_e end_block continue_block) fun_ctx.ll_builder in
    L.position_at_end continue_block fun_ctx.ll_builder;
    
    (* Codegen the right expression *)
    let g_right_e = right_e_generator fun_ctx in
    let incoming_bb_2 = L.insertion_block fun_ctx.ll_builder in
    (* Jump to the end block *)
    let _ = add_ending_instruction (L.build_br end_block) fun_ctx.ll_builder  in
    L.position_at_end end_block fun_ctx.ll_builder;

    (* Create the phi node to get the final value of the OR operation *)
    L.build_phi [(g_left_e, incoming_bb_1); (g_right_e, incoming_bb_2)] "phi_or_sc"


  (**
    Generates the LLVM code for the given expression
    @param: fun_ctx The context containing information about the current function and global state
    @param: expr The type-annotated expression node to generate
    @return: The LLVM value of the virtual register containing the result of the expression
  *)
  and gen_expr fun_ctx expr =
    match expr.node with
    | Ast.LV(lvalue) ->
      (* Get the lvalue by first doing a load of the address *)
      gen_lvalue ~address:false ~load_value:true fun_ctx lvalue
      
    | Ast.Assign(lvalue, expr) ->
      let g_expr = gen_expr fun_ctx expr in
      (* Based on the expression type, decide weather to load or not the lvalue *)
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
      (* Define closures in order to generate the lvalue only once *)
      let lvalue_generator = fun fun_ctx -> L.build_load g_lvalue "load" fun_ctx.ll_builder in
      let expr_generator = fun fun_ctx -> gen_expr fun_ctx expr in
      let g_binop = gen_binary_op binop lvalue.annot expr.annot lvalue_generator expr_generator fun_ctx in
      L.build_store g_binop g_lvalue fun_ctx.ll_builder |> ignore;
      g_binop

    | Ast.ILiteral(i) -> L.const_int int_type i
    | Ast.FLiteral(f) -> L.const_float float_type f
    | Ast.CLiteral(c) -> L.const_int char_type (Char.code c)
    | Ast.BLiteral(b) -> L.const_int bool_type (if b then 1 else 0)

    | Ast.UnaryOp(uop, e) -> 
      let g_e = gen_expr fun_ctx e in
      gen_unary_op uop e.annot g_e fun_ctx.ll_builder

    | Ast.Address(lvalue) -> 
      (* Get the address of the lvalue, so no previous loading is needed *)
      gen_lvalue ~address:true ~load_value:false fun_ctx lvalue

    | Ast.BinaryOp(binop, e1, e2) ->
      (* Define closures in order to support short circuiting for AND/OR *)
      let e1_generator = fun fun_ctx -> gen_expr fun_ctx e1 in
      let e2_generator = fun fun_ctx -> gen_expr fun_ctx e2 in
      gen_binary_op binop e1.annot e2.annot e1_generator e2_generator fun_ctx

    | Ast.Call(Some(cname), fun_name, exprs) ->
      begin
      let parameter_types = List.map (fun e -> e.annot) exprs in
      (* Mangle the name with parameters' type to support function overloading *)
      let mangled_fname = Utils.manglify_function fun_name parameter_types in
      (* Mangle the previous mangled name, with component name to support component scope *)
      let mangled_fname = if fun_name = "main" then fun_name else Utils.manglify_component cname mangled_fname in

      (* Lookup the function in the table and build the call with the parameters  *)
      let ll_fun = Symbol_table.lookup mangled_fname fun_ctx.global_ctx.table.functions in
      let g_exprs = Array.of_list (List.map (gen_expr fun_ctx) exprs) in
      match expr.annot with 
      | Ast.TVoid -> L.build_call ll_fun g_exprs "" fun_ctx.ll_builder
      | _ -> L.build_call ll_fun g_exprs "call" fun_ctx.ll_builder
      end
    | Ast.Call(None, _, _) -> ignore_case 5 "gen_expr: Call with None as qualifier cannot happend, because it is already handled in the linker"

    | Ast.IncDec(lvalue, inc_dec, pre_post) ->
      (* Get the lvalue *)
      let g_lvalue_address = gen_lvalue ~address:false ~load_value:false fun_ctx lvalue in
      let g_lvalue_value = L.build_load g_lvalue_address "load" fun_ctx.ll_builder in
      (* Apply increment or decrement to the value *)
      let g_result = match (lvalue.annot, inc_dec) with
      | Ast.TInt, Ast.Inc -> L.build_add g_lvalue_value llvm_one_i "inc" fun_ctx.ll_builder
      | Ast.TInt, Ast.Dec -> L.build_sub g_lvalue_value llvm_one_i "dec" fun_ctx.ll_builder
      | Ast.TFloat, Ast.Inc -> L.build_fadd g_lvalue_value llvm_one_f "finc" fun_ctx.ll_builder
      | Ast.TFloat, Ast.Dec -> L.build_fsub g_lvalue_value llvm_one_f "fdec" fun_ctx.ll_builder
      | _ -> ignore_case 6 "gen_expr: IncDec: wrong type cannot happen, because it is already checked in the semantic analysis"
      in
      (* Store the new value *)
      let _ = L.build_store g_result g_lvalue_address fun_ctx.ll_builder in
      (* Return the new result (i.e. pre-increment) or the old value (i.e. post-increment) *)
      match pre_post with
      | Ast.Pre -> g_result
      | Ast.Post -> g_lvalue_value


(**
    Given a context and an lvalue, this function generates LLVM code for the lvalue.
    An lvalue is a location in memory that can be assigned a value.
    @param: fun_ctx The context containing information about the current function and global state
    @param: lvalue The lvalue to generate code for
    @param: address A flag indicating whether the address of the lvalue should be returned, instead of its value
    @param: load_value A flag indicating whether the value stored at the address of the lvalue should be loaded
    @return: An Llvm.llvalue that represents the LLVM instruction generated by the function,
    this instruction is a load or a getelementptr operation depending on the input arguments.
  *)
  and gen_lvalue ?(address=false) ?(load_value=false) fun_ctx lvalue =
    (* A helper function to lookup variables in the symbol table *)
    let lookup cname id = 
      (* Look up the mangled name in the current function's symbol table *)
      let mangled_name = match cname with
        | Some(cname) -> Utils.manglify_component cname id
        | None -> id 
      in try 
        Symbol_table.lookup mangled_name fun_ctx.table
      with Symbol_table.NotFound(_) -> 
        (* Otherwise, look up the mangled name in the global symbol table *)
        let cname = match fun_ctx.global_ctx.cname with | Some(cname) -> cname | None -> "" in
        let mangled_name = Utils.manglify_component cname id in 
        Symbol_table.lookup mangled_name fun_ctx.global_ctx.table.variables
    in

    match lvalue.node with
    | Ast.AccVar(cname, id) -> 
      (* Lookup the variable in the symbol table and determine what to do based on the lvalue's type annotation *)
      let variable_sym = lookup cname id in
      begin
      match lvalue.annot with
      | Ast.TRef(_) -> 
        (* If we need just the address of the reference, return it directly *)
        if address then variable_sym
        else
          (* Otherwise, load the address of the reference from memory *)
          let ptr_value = L.build_load variable_sym "ref_ptr" fun_ctx.ll_builder in
          (* If we also need the value that the reference points to, load it from memory.
             Otherwise, just return the address of the reference *)
          if load_value then L.build_load ptr_value "ref_value" fun_ctx.ll_builder
          else ptr_value

      | Ast.TArray(_, Some _) -> 
        L.build_in_bounds_gep variable_sym [| llvm_zero_i; llvm_zero_i |] "getelem_ptr" fun_ctx.ll_builder

      (* If the lvalue is a primitive type (not a reference or an array), return its address or load its value from memory *)
      | _ -> 
        if load_value then L.build_load variable_sym "lv_value" fun_ctx.ll_builder
        else variable_sym
      end
    
    | Ast.AccIndex(lvalue, expr) -> 
      let variable_sym = match lvalue.node with | Ast.AccVar(cname, id) -> lookup cname id | _ -> ignore_case 3 "gen_lvalue: AccIndex: not AccVar" in
      (* Generate the expression for the index *)
      let g_index = gen_expr fun_ctx expr in

      match lvalue.annot with
      | Ast.TArray(typ, Some(_)) -> 
        (* Arrays with a size needs to be dereferenced with a beginning zero index. *)
        let array_ptr = L.build_in_bounds_gep variable_sym [| llvm_zero_i; g_index |] "array_ptr" fun_ctx.ll_builder in
        if load_value then
          match typ with
          | Ast.TRef(_) -> 
            let elem_address = L.build_load array_ptr "ref_ptr" fun_ctx.ll_builder in 
            L.build_load elem_address "ref_value" fun_ctx.ll_builder
          | _ -> L.build_load array_ptr "array_elem" fun_ctx.ll_builder
        else
          array_ptr
      | Ast.TRef(_)
      | Ast.TArray(_, None) ->
        (* Arrays without are dereferenced like pointer to a contiguos memory area. *)
        let array_ptr = L.build_load variable_sym "array_ptr_unbound" fun_ctx.ll_builder in
        let address = L.build_in_bounds_gep array_ptr [| g_index |] "array_ptr_inbound" fun_ctx.ll_builder in
        if load_value then
          L.build_load address "array_ptr" fun_ctx.ll_builder
        else
          address
      | _ -> ignore_case 6 "gen_lvalue: AccIndex: type not supported cannot happen because of type checking"


  (**
    Given a context and a statement, this function generates LLVM code for the statement.
    @param: fun_ctx The context containing information about the current function and global state
    @param: stmt The statement to generate code for
    @return: A boolean indicating whether to continue generating code for the current function
  *)
  let rec gen_stmt fun_ctx stmt = 
    match stmt.node with
    | Ast.If(e, s1, s2) ->
      let then_block  = L.append_block ll_ctx "if_then"  fun_ctx.ll_value in
      let else_block  = L.append_block ll_ctx "if_else"  fun_ctx.ll_value in
      let merge_block = L.append_block ll_ctx "if_merge" fun_ctx.ll_value in

      (* Generate code for the condition expression *)
      let g_e = gen_expr fun_ctx e in
      (* Create a conditional branch based on the value of the condition *)
      let _ = L.build_cond_br g_e then_block else_block fun_ctx.ll_builder in

      (* Move the builder to the end of the then block and generate code for the then branch *)
      let _ = L.position_at_end then_block fun_ctx.ll_builder in
      let _ = gen_stmt fun_ctx s1 in
      (* Create an unconditional branch from the end of the then block to the merge block *)
      let _ = add_ending_instruction (L.build_br merge_block) fun_ctx.ll_builder in

      (* Move the builder to the end of the else block and generate code for the else branch *)
      let _ = L.position_at_end else_block fun_ctx.ll_builder in
      let _ = gen_stmt fun_ctx s2 in
      (* Create an unconditional branch from the end of the else block to the merge block *)
      let _ = add_ending_instruction (L.build_br merge_block) fun_ctx.ll_builder in 

      (* Move the builder to the end of the merge block *)
      let _ = L.position_at_end merge_block fun_ctx.ll_builder in
      true
    
    | Ast.While(e, s1) ->
      let condition = L.append_block ll_ctx "while_condition" fun_ctx.ll_value in
      let body = L.append_block ll_ctx "while_body" fun_ctx.ll_value in
      let continue = L.append_block ll_ctx "while_continue" fun_ctx.ll_value in

      (* Transfer control to the condition block *)
      let _ = add_ending_instruction (L.build_br condition) fun_ctx.ll_builder in
      (* Set the builder position to the end of the condition block *)
      let _ = L.position_at_end condition fun_ctx.ll_builder in
      (* Generate code for the loop condition that checks whether to continue the loop or exit *)
      let g_e = gen_expr fun_ctx e in
      let _ = add_ending_instruction (L.build_cond_br g_e body continue) fun_ctx.ll_builder in

      (* Set the builder position to the end of the body block and generate code for the body *)
      let _ = L.position_at_end body fun_ctx.ll_builder in
      let _ = gen_stmt fun_ctx s1 in
      (* Transfer control back to the condition block *)
      let _ = add_ending_instruction (L.build_br condition) fun_ctx.ll_builder in

      (* Set the builder position to the end of the continue block *)
      let _ = L.position_at_end continue fun_ctx.ll_builder in
      true
    
    | Ast.DoWhile(e, s) -> 
      let condition = L.append_block ll_ctx "dowhile_condition" fun_ctx.ll_value in
      let body = L.append_block ll_ctx "dowhile_body" fun_ctx.ll_value in
      let continue = L.append_block ll_ctx "dowhile_continue" fun_ctx.ll_value in

      (* Transfer control to the body block *)
      let _ = add_ending_instruction (L.build_br body) fun_ctx.ll_builder in
      (* Set the builder position to the start of the body block *)
      let _ = L.position_at_end body fun_ctx.ll_builder in
      (* Generate code for the body of the loop and trasnfer control to the condition block *)
      let _ = gen_stmt fun_ctx s in
      let _ = add_ending_instruction (L.build_br condition) fun_ctx.ll_builder in

      (* Set the builder position to the start of the condition block *)
      let _ = L.position_at_end condition fun_ctx.ll_builder in
      (* Generate code for the loop condition that checks whether to continue the loop or exit *)
      let g_e = gen_expr fun_ctx e in
      let _ = add_ending_instruction (L.build_cond_br g_e body continue) fun_ctx.ll_builder in

      (* Set the builder position to the start of the continue block *)
      let _ = L.position_at_end continue fun_ctx.ll_builder in
      true

    | Ast.For(e1, e2, e3, s) -> 
      let condition = L.append_block ll_ctx "for_condition" fun_ctx.ll_value in
      let body = L.append_block ll_ctx "for_body" fun_ctx.ll_value in
      let continue = L.append_block ll_ctx "for_continue" fun_ctx.ll_value in

      (* Generate code for the initialization expression *)
      let _ = match e1 with 
      | Some(e) -> gen_expr fun_ctx e
      | None ->  L.const_int int_type 0
      in

      (* Transfer control to the condition block *)
      let _ = add_ending_instruction (L.build_br condition) fun_ctx.ll_builder in
      (* Set the builder position to the end of the condition block *)
      let _ = L.position_at_end condition fun_ctx.ll_builder in
      (* Generate code for the loop condition that checks whether to continue the loop or exit *)
      let g_e2 = match e2 with
      | Some(e) -> gen_expr fun_ctx e
      | None -> llvm_one_i
      in
      let _ = add_ending_instruction (L.build_cond_br g_e2 body continue) fun_ctx.ll_builder in

      (* Set the builder position to the end of the body block and generate the body code *)
      let _ = L.position_at_end body fun_ctx.ll_builder in
      let _ = gen_stmt fun_ctx s in
      (* Generate code for the increment expression *)
      let _ = match e3 with
      | Some(e) -> gen_expr fun_ctx e
      | None -> L.const_int int_type 0
      in
      (* Transfer control back to the condition block *)
      let _ = add_ending_instruction (L.build_br condition) fun_ctx.ll_builder in

      (* Set the builder position to the end of the continue block *)
      let _ = L.position_at_end continue fun_ctx.ll_builder in
      true

    | Ast.Expr(e) -> let _ = gen_expr fun_ctx e in (); true

    | Ast.Return(None) ->
      add_ending_instruction (L.build_ret_void) fun_ctx.ll_builder; 
      false (* Stop generating code for the rest of the function *)
    
    | Ast.Return(Some(expr)) ->
      add_ending_instruction (L.build_ret (gen_expr fun_ctx expr)) fun_ctx.ll_builder; 
      false (* Stop generating code for the rest of the function *)
    
    | Ast.Block(stmtordec) -> 
      (* Create a new scope for the block *)
      let fun_ctx = {fun_ctx with table = Symbol_table.begin_block fun_ctx.table} in
      let continue_codegen = gen_stmtordec fun_ctx in
      (* Execute the codegen over the list of stmtordec. The List.for_all checks that
         the predicates holds for all the elements. But it uses short-circuiting for 
         the "and" evaluation, so at the first false returned by the codegen over a stmt,
         it stops the codegen. *)
      List.for_all continue_codegen stmtordec

    | Ast.Skip -> false

  
  (**
    Given a function context and a statement or declaration, generate the LLVM IR code.
    @param: fun_ctx The context containing information about the current function and global state
    @param stmtordec The statement or declaration to generate code for
    @return A boolean indicating whether to continue generating code for the current function
  *)
  and gen_stmtordec fun_ctx stmtordec =
    match stmtordec.node with 
    | Ast.Stmt(stmt) ->  gen_stmt fun_ctx stmt
    | Ast.LocalDecl((id, typ), init) -> 
      (* Define a local variable *)
      let local_var = build_alloca typ id fun_ctx.ll_builder in
      (* Initialize the variable with an undef value or with the optionally provided expression. *)
      let value = match init with 
      | None -> var_init typ
      | Some(expr) -> gen_expr fun_ctx expr
      in
      L.build_store value local_var fun_ctx.ll_builder |> ignore;
      (* Keep track of the local variable in the symbol table *)
      Symbol_table.add_entry id local_var fun_ctx.table |> ignore;
    true
      

  (**
    Given a function context and a function and the parameter tuple (name, type) with the argument, generate the LLVM IR code.
    @param: ctx The context containing information about the current function and global state
    @param: param The parameter tuple (name, type) with the argument
    @param: argument The argument to generate code for 
  *)
  let gen_local_args (fun_ctx : fun_context) (id, typ) argument = 
    let local_arg = build_alloca typ id fun_ctx.ll_builder in
    let _ = Symbol_table.add_entry id local_arg fun_ctx.table in
    (* Load the function argument in the local variable *)
    L.build_store argument local_arg fun_ctx.ll_builder |> ignore


  (**
    Given a global context and a function, generate the LLVM IR code.
    @param: ctx The context containing information about the current function and global state
    @param: f The fun_decl to generate code for
    @precondition The function has already been declared in the symbol table
  *)
  let gen_fundecl (ctx : global_context) f =
    let ll_fun = Symbol_table.lookup f.fname ctx.table.functions in
    let ll_builder = L.builder_at_end ll_ctx (L.entry_block ll_fun) in
    (* Create a new scope for the function *)
    let fun_table = Symbol_table.begin_block ctx.table.variables in
    let fun_ctx = {ll_builder = ll_builder; ll_value = ll_fun; table = fun_table; global_ctx = ctx;} in
    (* Allocate the function's arguments into the stack *)
    let ll_params = Array.to_list (L.params ll_fun) in
    let _ = List.iter2 (gen_local_args fun_ctx) f.formals ll_params in
    (* Codegen the body *)
    let _ = match f.body with | Some(body) -> gen_stmt fun_ctx body | None -> ignore_err 1 in
    (* If missing add return type based on the function's return type *)
    let terminator = match f.rtype with Ast.TVoid -> L.build_ret_void | _ -> L.build_ret (var_init f.rtype) in
    add_ending_instruction terminator ll_builder |> ignore

  (**
    Given a global context and a definition, generate the LLVM IR code.
    @param: ctx The context containing information about the current function and global state
    @param: definition The definition to generate code for
  *)
  let gen_definition ctx definition =
  match definition.node with
  | Ast.FunDecl(f) -> (gen_fundecl ctx f)
  | Ast.VarDecl(_) -> ()


  (** 
    Given a global context and a component, generate the LLVM IR code.
    @param: ctx The context containing information about the current function and global state
    @param: component The component to generate code for
  *)
  let gen_component ctx component =
    match component.node with
    | Ast.ComponentDecl({cname; uses=_; provides=_; definitions}) ->
      let ctx = {ctx with cname = Some(cname)} in
      List.iter (gen_definition ctx) definitions 
    
  (**
    Generates the LLVM IR code for the global constructors function used to initialize global variables with epxressions.
    In general is not needed, but this provides an easy way to change the code and allow for global variables
    to be initialized with expressions that are not only compile-time constants. 
    @param: ll_module The LLVM module to add the function to
    @return: The function declaration with its builder
  *)
  let gen_global_ctors_function ll_module = 
    (* Define a void type for the function, pointer to it and pointer to a string that contains the name*)
    let ll_fun_typ = L.function_type void_type [||] in
    let ll_ptr_to_fun = L.pointer_type ll_fun_typ in
    let ll_ptr_to_char = L.pointer_type (L.i8_type ll_ctx) in
    
    (* Define the functoin with a maximum priority for the evaluation order *)
    let fname = "__llvm__global_ctors" in 
    let ftype = ast_to_llvm (Ast.TFun([], Ast.TVoid)) in
    let fun_priority = L.const_int int_type 65535 in
    let fun_decl = L.define_function fname ftype ll_module in
    (* The function is not visible outside this module *)
    ignore(L.set_linkage L.Linkage.Internal fun_decl);
    let fun_builder = L.builder_at_end ll_ctx (L.entry_block fun_decl) in

    (* Create a struct to hold the priority, pointer to the function and pointer to the name of it *)
    let lltype_struct = L.struct_type ll_ctx [| int_type; ll_ptr_to_fun; ll_ptr_to_char |] in
    (* Create an instance of the struct with the above values, and insert it into the global_ctors array *)
    let fun_struct = L.const_struct ll_ctx [| fun_priority; fun_decl; L.const_stringz ll_ctx fname |] in
    let global_ctors_array = L.define_global "llvm.global_ctors" (L.const_array lltype_struct [| fun_struct |]) ll_module in
    (* Allow the merge of multiple llvm.global_ctors in one *)
    ignore(L.set_linkage L.Linkage.Appending global_ctors_array);

    (fun_decl, fun_builder)
end
(* END CODEGEN SUB-MODULE*)


(* DECLARATION SUB-MODULE*)

module Declare = struct
  
  (**
    Given a global context and a component, declare the component's functions and variables in the symbol table.
    @param: ctx The context containing information about the global state
    @param: cname The name of the component
    @param: member The member to declare
    @return: The member with its name mangled
  *)
  let declare_member ctx cname member =
    let node = match member.node with
    | Ast.FunDecl f ->
        let ll_typ = ast_to_llvm member.annot in
        let parameter_types = List.map (fun (_, typ) -> typ) f.formals in
        let mangled_fname = Utils.manglify_function f.fname parameter_types in
        let mangled_name = if f.fname = "main" then f.fname else Utils.manglify_component cname mangled_fname in
        let ll_fun = L.define_function mangled_name ll_typ ctx.ll_module in
        Symbol_table.add_entry mangled_name ll_fun ctx.table.functions |> ignore;
        Ast.FunDecl {f with fname = mangled_name}

    | Ast.VarDecl((id, typ), init) ->
        let mangled_name = Utils.manglify_component cname id in
        let (ll_value, ll_builder) = ctx.global_ctors in 
        let ll_var = L.define_global mangled_name (var_init typ) ctx.ll_module in
      
        let _ = match init with
        | None -> var_init typ
        | Some(expr) -> 
          let fun_ctx = {ll_builder = ll_builder; ll_value = ll_value; table = Symbol_table.empty_table (); global_ctx = ctx;} in
          let g_expr = Codegen.gen_expr fun_ctx expr in 
          L.build_store g_expr ll_var fun_ctx.ll_builder
        in
        Symbol_table.add_entry mangled_name ll_var ctx.table.variables |> ignore;

        Ast.VarDecl ((mangled_name, typ), init)
    
    in {member with node}

  
  let declare_component ctx component =
    let node = match component.node with
    | Ast.ComponentDecl { cname; uses; provides; definitions } ->
        let definitions = List.map (declare_member ctx cname) definitions in
        Ast.ComponentDecl { cname; uses; provides; definitions }
    in { component with node }
    

  (**
    Helper function to declare a function in the LLVM module from the signatures of the STD library.
    @param: ctx The context of the current compilation unit
    @param: cname The name of the component that is being compiled
    @param: fname The name of the function to declare
    @param: ftyp The type of the function to declare
  *)
  let declare_function ctx cname (fname, ftyp) =
    let parameter_types = match ftyp with Ast.TFun (p, _) -> p | _ -> [] in
    let mangled_fname = Utils.manglify_function fname parameter_types in
    let mangled_name = Utils.manglify_component cname mangled_fname in
    let ll_fun = L.declare_function mangled_name (ast_to_llvm ftyp) ctx.ll_module in
    Symbol_table.add_entry mangled_name ll_fun ctx.table.functions |> ignore

end
(* END DECLARATION SUB-MODULE*)


(* ================================== MAIN FUNCTION ==================================*)

(**
  Given an AST annotated with types, generate the LLVM IR for the compilation unit.
  @param: ast The AST of the compilation unit
  @return: The LLVM module containing the IR for the compilation unit
*)
let to_llvm_module ast =
  let components = match ast with | Ast.CompilationUnit { interfaces = _; components = co; _ } -> co in
  let llmodule = L.create_module ll_ctx "mcomp_global" in
  let global_ctors = Codegen.gen_global_ctors_function llmodule in 
  let global_table =
  {
    functions = Symbol_table.empty_table ();  
    variables = Symbol_table.empty_table ();
  }
  in
  let ctx = { ll_module = llmodule; table = global_table; cname = None; global_ctors} in

  (* Declare the prelude functions *)
  List.iter (Declare.declare_function ctx "prelude") Mcomp_stdlib.prelude_signature;
  

  (* Declare in advance the members of the components:
     - functions, so that functions can reference each other
     - global variables, so that functions can referece them 
  *)
  let components = List.map (Declare.declare_component ctx) components in
  (* Add return void instruction to the ctors after the initializations of global variables *)
  let _ = L.build_ret_void (snd global_ctors) in

  (* Codegen the component *)
  List.iter (Codegen.gen_component ctx) components;

  (* Debug *)
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

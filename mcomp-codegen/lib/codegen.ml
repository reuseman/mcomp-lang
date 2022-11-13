module L = Llvm

let ignore_case number message = failwith (Printf.sprintf "Error %d. This should not happen. " number ^ message)
let ignore number = ignore_case number ""

(* TODO: should i have an exception? *)


let llcontext = L.global_context ()

(* Get references to the LLVM types *)
let int_type = L.i32_type llcontext
let bool_type = L.i1_type llcontext
let char_type = L.i8_type llcontext
let void_type = L.void_type llcontext


let to_llvm_module _ = 
  let llvm_context = L.global_context () in
  let llvm_module = L.create_module llvm_context "test2" in
  Printf.printf "Module: %s" (L.string_of_llmodule llvm_module);
  llvm_module
open Ast

let create_hashtbl list =  
  let hashtbl = Hashtbl.create (List.length list) in
  List.iter (fun (key, tkn) -> Hashtbl.add hashtbl key tkn) list;
  hashtbl


(* https://en.wikipedia.org/wiki/Name_mangling *)
(* This is used to support function with the same name belonging to different components *)
let manglify_component component identifier = 
  let component = String.lowercase_ascii component in
  let identifier = String.lowercase_ascii identifier in
  "_" ^ component ^ "_" ^ identifier


(* This is used to support functoin overloading *)
(* let manglify_function fname args_type = 
  let rec aux_serializer = function 
  | TInt -> "i"
  | TFloat -> "f"
  | TBool -> "b"
  | TChar -> "c"
  | TArray (t, None) -> Printf.sprintf "%s[]" (aux_serializer t)
  | TArray (t, _) -> Printf.sprintf "%s[]" (aux_serializer t)
  | TRef t -> Printf.sprintf "r%s" (aux_serializer t) 
  | TVoid -> "v"
  | _ -> failwith "Trying to manglify a not supported type"
  in
  let args_type_serialized = List.map aux_serializer args_type in
  match args_type with 
  | [] -> Printf.sprintf "N_%s_T_v" fname
  | _ -> Printf.sprintf "N_%s_T_%s" fname (String.concat "_" args_type_serialized) *)


  let manglify_function ?(is_call=false) fname args_type = 
    let rec aux_serializer = function 
    | TInt -> "i"
    | TFloat -> "f"
    | TBool -> "b"
    | TChar -> "c"
    | TArray (t, None) -> Printf.sprintf "%s[]" (aux_serializer t)
    | TArray (t, _) -> Printf.sprintf "%s[]" (aux_serializer t)
    | TRef (t, is_address) -> 
      if is_call then
        match is_address with
        | true -> Printf.sprintf "r%s" (aux_serializer t)   (* If calling with address, then search for the function with _T_r{type}*)
        | false -> Printf.sprintf "%s" (aux_serializer t)   (* Otherwise calling with a reference type, auto-reference and call _T_{type}*)
      else 
        Printf.sprintf "r%s" (aux_serializer t)
    | TVoid -> "v"
    | _ -> failwith "Trying to manglify a not supported type"
    in
    let args_type_serialized = List.map aux_serializer args_type in
    match args_type with 
    | [] -> Printf.sprintf "N_%s_T_v" fname
    | _ -> Printf.sprintf "N_%s_T_%s" fname (String.concat "_" args_type_serialized)
  
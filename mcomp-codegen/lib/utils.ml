let create_hashtbl list =  
  let hashtbl = Hashtbl.create (List.length list) in
  List.iter (fun (key, tkn) -> Hashtbl.add hashtbl key tkn) list;
  hashtbl
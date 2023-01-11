open Ast 

let prelude_signature = [
  (* Print is to print a primitive and a newline *)
  "print", TFun([TInt],TVoid);
  "print", TFun([TFloat],TVoid);
  "print", TFun([TBool],TVoid);
  "print", TFun([TChar],TVoid);

  (* Put is to print a primitive without the newline *)
  "put", TFun([TInt],TVoid);
  "put", TFun([TFloat],TVoid);
  "put", TFun([TBool],TVoid);
  "put", TFun([TChar],TVoid);

  (* Get is to read a primitive *)
  "getint", TFun([], TInt);
  "getfloat", TFun([], TFloat);
  "getbool", TFun([], TBool);
  "getchar", TFun([], TChar);
]

let app_signature = [
  "main", TFun([],TInt)
]
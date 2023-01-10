open Ast 

let prelude_signature = [
  "print", TFun([TInt],TVoid);
  "print_float", TFun([TFloat],TVoid);
  "getint", TFun([], TInt)
]

let app_signature = [
  "main", TFun([],TInt)
]
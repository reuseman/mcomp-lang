{
  (* OCaml declaration*)   
  open Parser

  exception Lexing_error of Location.lexeme_pos * string

  let raise_syntax_error lexbuf msg = 
    raise (Lexing_error ((Location.to_lexeme_position lexbuf), msg))

  (* Identifier and number constraints *)
  let identifier_max_length = 64
  let integer_max_value     = 2147483647 
  let integer_min_value     = -2147483648
  let float_max_value       = Float.max_float
  let float_min_value       = Float.min_float

  (* Keywords *)

  let keywords = Utils.create_hashtbl
    [("interface",   INTERFACE);
      ("uses",       USES);
      ("provides",   PROVIDES);
      ("component",  COMPONENT);
      ("connect",    CONNECT);
      ("def",        DEF);
      ("var",        VAR);
      ("return",     RETURN);
      ("if",         IF);
      ("else",       ELSE);
      ("for",        FOR);
      ("while",      WHILE);
      ("do",         DO);
      ("int",        TYPE_INT);
      ("float",      TYPE_FLOAT);
      ("char",       TYPE_CHAR);
      ("void",       TYPE_VOID);
      ("bool",       TYPE_BOOL);
      ("true",       BOOL_VALUE(true));
      ("false",      BOOL_VALUE(false));
    ]

    (* Check constraints *)
    let int32_check_interval num lexbuf = 
      if num >= integer_min_value && num <= integer_max_value then
        INT_VALUE(num)
      else
        let msg = Printf.sprintf "Syntax error: The Integer must be a 32-bit number, between %d and %d." integer_min_value integer_max_value in
        raise_syntax_error lexbuf msg

    let float32_check_interval num lexbuf =
      match classify_float num with
      | Float.FP_normal | Float.FP_subnormal | Float.FP_zero -> 
        FLOAT_VALUE(num)
      | Float.FP_infinite | Float.FP_nan -> 
        let msg = Printf.sprintf "Syntax error: The Float must be a 32-bit number, between %f and %f." float_min_value float_max_value in
        raise_syntax_error lexbuf msg

    let identifier_check_max_lenght id lexbuf =
      if String.length id <= identifier_max_length then
        ID id
      else
        let msg = Printf.sprintf "Syntax error: The Identifier must be less or equal than %d characters." identifier_max_length in
        raise_syntax_error lexbuf msg
}


(* Declaration of regular expressions *)
let digit          = ['0'-'9']
let alpha          = ['a'-'z' 'A'-'Z']

let whitespace     = [' ' '\t']+
let newline        = '\r' | '\n' | "\r\n"

let identifier     = (alpha | '_') (alpha | digit | '_')*
let number_base_10 = digit+
let number_base_16 = "0x"(digit | ['0'-'9' 'a'-'f' 'A'-'F'])+
let exponent      = ['e' 'E'] ['+' '-']? digit+
let float         = (digit digit* '.' digit* | digit* '.' digit digit*) exponent?


(* Declaration of scanner functions *)
rule next_token = parse
  | whitespace
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }

  (* Numbers *)
  | number_base_10
  | number_base_16 as num { int32_check_interval (int_of_string num) lexbuf  }
  | float as num { float32_check_interval (float_of_string num) lexbuf }
  
  (* Chars *)
  | "\'" {  character lexbuf  }
  
  (* Identifiers *)
  | identifier as id  
    {
      match Hashtbl.find_opt keywords id with
      | Some tkn -> tkn
      | None -> identifier_check_max_lenght id lexbuf
    }

  (* Comments *)
  | "//" {  single_line_comment lexbuf  }
  | "/*" {  multi_line_comment lexbuf   }

  (* Binary operators *)
  | '+'   { PLUS  }
  | '-'   { MINUS }
  | '*'   { TIMES }
  | '/'   { DIV   }
  | '%'   { MOD   }
  | "=="  { EQ    }
  | "!="  { NE    }
  | '<'   { LT    }
  | "<="  { LE    }
  | '>'   { GT    }
  | ">="  { GE    }
  | "&&"  { AND   }
  | "||"  { OR    }

  (* Assign binary operators *)
  | "+="  { PLUS_ASSIGN  }
  | "-="  { MINUS_ASSIGN }
  | "*="  { TIMES_ASSIGN }
  | "/="  { DIV_ASSIGN   }
  | "%="  { MOD_ASSIGN   }

(* Single character symbols *)
  | "!" { NOT       }
  | "{" { LBRACE    }
  | "}" { RBRACE    }
  | "(" { LPAREN    }
  | ")" { RPAREN    }
  | "[" { LBRACKET  }
  | "]" { RBRACKET  }
  | '.' { DOT       }
  | ':' { COLON     }
  | ',' { COMMA     }
  | ';' { SEMICOLON }
  | '&' { REFERENCE }
  | "=" { ASSIGN    }

  (* Two character symbols *)
  | "++" { PLUSPLUS     }
  | "--" { MINUSMINUS   }
  | "<-" { ARROW        }

  (* End of file *)
  | eof { EOF }
  
  (* Unrecognized token *)
  | _ as unrecognized { raise_syntax_error lexbuf (Printf.sprintf "Syntax error: The token \"%c\" is not recognized." unrecognized) }


(* Char helpers *)
and character = parse 
  (* Raise an error if the apostrophe has been opened, but immediately after there is the EOF, the newline or again the apostrophe *)
  | eof | newline | "\'" { raise_syntax_error lexbuf "Syntax error: The Char must be closed with a \"'\"." }
  (* Otherwise, parse the character and then check if it's closed *)
  | "" { let c = CHAR_VALUE(character_parser lexbuf) in character_close lexbuf; c}

and character_close = parse
  | "\'" {  }
  | _ {raise_syntax_error lexbuf ("Syntax error: The Char is closed with an invalid character. It must be a \"'\".")}

(* For special characters the escape is needed, so that the Ocaml compilers know
 that they should be treated as characters rather than a control command *)
and character_parser = parse 
  | "\\'"   { '\''    }
  | "\""    { '\"'    }
  | "\\"    { '\\'    }
  | "\\n"   { '\n'    }
  | "\r"    { '\r'    }
  | "\t"    { '\t'    }
  | "\b"    { '\b'    }
  | "\\f"   { '\x0c'  }
  | "\\v"   { '\x0b'  }
  | "\\0"   { '\x00'  }
  | "\xFF"  { '\xFF'  }
  | _ as c  { c       }
  

(* Comment helpers *)
and single_line_comment = parse
  | newline { Lexing.new_line lexbuf; next_token lexbuf }
  | eof     { EOF }
  | _       { single_line_comment lexbuf }

and multi_line_comment = parse
  | newline { Lexing.new_line lexbuf; multi_line_comment lexbuf }
  | "*/"    { next_token lexbuf }
  | eof     { raise_syntax_error lexbuf "Syntax error: The comment is not closed." }
  | _       { multi_line_comment lexbuf }

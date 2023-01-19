%{
    open Ast

    let ($$) a b = Ast.annotate_node a (Location.to_code_position b)
%} 

/* Token declarations */
%token EOF

%token NEG
%token NOT "!"

// Increment and decrement operators (both prefix and postfix)
%token PLUSPLUS "++"  MINUSMINUS "--"

// Binary operators
%token PLUS "+"   MINUS "-"   TIMES "*"   DIV "/"   MOD "%"

// Assignment binary operators
%token PLUS_ASSIGN "+="   MINUS_ASSIGN "-="   TIMES_ASSIGN "*="   DIV_ASSIGN "/="   MOD_ASSIGN "%="

// Logical operators
%token LT "<"   GT ">"  LE "<="   GE ">="   EQ "=="   NE "!="
%token AND "&&" OR "||"

// Symbols
%token LBRACE   "{"   RBRACE    "}"
%token LPAREN   "("   RPAREN    ")"
%token LBRACKET "["   RBRACKET  "]"

%token DOT "."  COLON ":"   COMMA ","   SEMICOLON ";"
%token REFERENCE "&"
%token ASSIGN "="
%token ARROW "<-"

%token <int>    INT_VALUE
%token <float>  FLOAT_VALUE
%token <char>   CHAR_VALUE
%token <bool>   BOOL_VALUE
%token <string> ID

%token INTERFACE "interface"
%token USES "uses"
%token PROVIDES "provides"
%token COMPONENT "component"
%token CONNECT "connect"

%token DEF "def"  VAR "var"     RETURN "return"
%token IF "if"    ELSE "else"   FOR "for"         WHILE "while"       DO "do"

// Types
%token TYPE_INT "int"   TYPE_FLOAT "float"   TYPE_CHAR "char"  TYPE_VOID "void"  TYPE_BOOL "bool"


/*  Precedence and associativity specification

    For the precedence, the priority is implicitly determined
    by the order of declaration. The lower is declared, the higher the priority.
 */
%nonassoc NO_ELSE
%nonassoc ELSE

%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIV_ASSIGN MOD_ASSIGN
// Assignment is done from right to left, so x = y = z is equivalent to x = (y = z)

%left OR
%left AND
// OR and AND are left-associative, so a || b || c is equivalent to (a || b) || c

%left EQ NE

%nonassoc LT GT LE GE 
// Comparison is not associative, so a < b < c needs be writtena as (a < b) && (b < c)

%left PLUS MINUS
%left TIMES DIV MOD
// a + b * c is equivalent to a + (b * c) because of the priority

%right MINUSMINUS

%nonassoc NEG // unary minus
%nonassoc NOT


/* Start symbol */
%start compilation_unit
%type <Ast.located_compilation_unit> compilation_unit 
%% 


/* Grammar Specification */
compilation_unit:
  | td=top_decl* EOF       
    { 
      let rec build_unit inter_list comp_list conn_list = function
        | [] -> (Ast.CompilationUnit({ interfaces = inter_list; components = comp_list; connections = conn_list; }))
        | Ast.Interface(i) :: xs -> build_unit (i :: inter_list) comp_list conn_list xs
        | Ast.Component(c) :: xs -> build_unit inter_list (c :: comp_list) conn_list xs
        | Ast.Connection(c) :: xs -> build_unit inter_list comp_list (c @ conn_list) xs 
        in build_unit [] [] [] td
    }                   
;


// 2 - TopDecl ::= "interface" ID "{" IMemberDecl+ "}" 
//          | "component" ID ProvideClause? UseClause? "{" CMemberDecl+ "}"
//          | "connect" Link ';'
//          | "connect" "{" (Link ";")* "}"
top_decl:
  | i=interface
    { Ast.Interface(i) }
  | co=component
    { Ast.Component(co) }
  | cn=connection
    { Ast.Connection(cn) }
;

interface:
  | "interface" id=ID "{" imd=i_member_decl+ "}"
    { Ast.InterfaceDecl({ iname = id; declarations = imd}) $$ $loc  }
;

component:
  | "component" id=ID pc=provide_clause? uc=use_clause? "{" cmd=c_member_decl+ "}"
    {
      let to_list clause = match clause with None -> [] | Some l -> l
      in Ast.ComponentDecl({ cname = id; provides = (to_list pc); uses = (to_list uc); definitions = cmd; }) $$ $loc
    }
;

connection:
  | "connect" l=link ";"?
    { [l] }
  | "connect" "{" l=links "}"
    { l }
;


// 3 - Link ::=  ID "." ID "<-" ID "." ID 
link:
  | component1=ID "." method1=ID "<-" component2=ID "." method2=ID
    { Ast.Link(component1, method1, component2, method2)  }
;

links:
  | { []  }
  | l=link ";" ls=links
    { l :: ls }
;


// 4 - IMemberDecl ::= "var" VarSign ";" | FunProto ";"
i_member_decl:
  | "var" vs=var_sign ";"
    { Ast.VarDecl(vs, None) $$ $loc }
  | fp=fun_proto ";"
    { Ast.FunDecl(fp) $$ $loc  }
;


// 5 - ProvideClause ::= "provides" (ID ",")* ID 
provide_clause:
  | "provides" pc=separated_nonempty_list(",", ID)
    { pc  }
;


// 6 - UseClause ::= "uses" (ID ",")* ID
use_clause:
  | "uses" uc=separated_nonempty_list(",", ID)
    { uc  }
;


// 7 - VarSign ::= ID ":" Type
var_sign:
  | id=ID ":" ct=complex_type
    { (id, ct) }
;


// 8 - FunProto ::= "def" ID "("((VarSign ",")* VarSign)? ")" (":" BasicType)? 
fun_proto:
  | "def" id=ID "(" f=separated_list(",", var_sign) ")"
    { { rtype = TVoid; fname = id; formals = f; body = None; } }
  | "def" id=ID "(" f=separated_list(",", var_sign) ")" ":" bt=basic_type
    { { rtype = bt; fname = id; formals = f; body = None; } }
;


// 9 - CMemberDecl ::= "var" VarSign ";"  | FunDecl
c_member_decl:
  | "var" vs=var_sign ";"
    { Ast.VarDecl(vs, None) $$ $loc }
  | "var" vs=var_sign "=" e=expr ";"
    { Ast.VarDecl(vs, Some(e)) $$ $loc }
  | fd=fun_decl
    { Ast.FunDecl(fd) $$ $loc }
;


// 10 - Fundecl ::= FunProto Block
fun_decl:
  | fp=fun_proto b=block 
    { {fp with body=(Some b)}  }
;


// 11 - Block ::= "{" (Stmt | "var" VarSign ";")* "}"
block:
  | bl=delimited("{", block_line*, "}")
    { Ast.Block(bl) $$ $loc }
;

block_line:
  | s=stmt
    { Ast.Stmt(s) $$ $loc }
  | "var" vs=var_sign ";"
    { Ast.LocalDecl(vs, None) $$ $loc }
  | "var" vs=var_sign "=" e=expr ";"
    { Ast.LocalDecl(vs, Some(e)) $$ $loc }
;


// 12 - Type ::= BasicType | Type "[" "]" | Type "[" INT "]" | "&" BasicType
complex_type:
  | bt=basic_type
    { bt  }
  | ct=complex_type "[" "]" 
    { Ast.TArray(ct, None)  }
  | ct=complex_type size=delimited("[", INT_VALUE, "]")
    { Ast.TArray(ct, Some size) }
  | "&" bt=basic_type
    { Ast.TRef(bt, false)  }
;


// 13 - BasicType ::= "int" | "float" | "char" | "void" | "bool"  
basic_type:
  | "int"   { TInt  }
  | "float" { TFloat }
  | "char"  { TChar }
  | "bool"  { TBool }
  | "void"  { TVoid }
;


// 14 - Stmt ::= "return" Expr? ";" | Expr? ";" | Block | "while" "(" Expr ")" Stmt 
//            | "if" "(" Expr ")" Stmt "else" Stmt  | "if" "(" Expr ")" Stmt
//            | "for" "(" Expr? ";" Expr? ";" Expr? ")" Stmt
stmt:
  | "return" e=expr? ";"
    { Ast.Return(e) $$ $loc }
  | ";"
    { Ast.Skip $$ $loc }
  | e=expr ";"
    { Ast.Expr(e) $$ $loc }
  | b=block
    { b }
  | "while" "(" e=expr ")" s=stmt
    { Ast.While(e, s) $$ $loc }
  | "do" s=stmt "while" "(" e=expr ")" ";"
    { Ast.DoWhile(e, s) $$ $loc }

  // According to the precedence priority, first try to match the "if-else" rule, otherwise match the "if" rule
  | "if" "(" e=expr ")" s1=stmt "else" s2=stmt
    { Ast.If(e, s1, s2) $$ $loc }
  | "if" "(" e=expr ")" s1=stmt %prec NO_ELSE  // the prec here forces the precedence level of the production rule to be the one of the NO_ELSE, rathen then the precedence of the rightmost terminal symbol
    { Ast.If(e, s1, Ast.Skip $$ $loc) $$ $loc }
  | "for" "(" e1=expr? ";" e2=expr? ";" e3=expr? ")" s=stmt
    { Ast.For(e1, e2, e3, s) $$ $loc }
;


// 15 - Expr ::= INT | FLOAT | CHAR | BOOL | "(" Expr ")" | "&" LValue | LValue "=" Expr | "!" Expr 
//              | ID "(" ((Expr ",")* Expr)? ")" | LValue | "-" Expr | Expr BinOp Expr
//              | LValue BinOp Expr | "++" LValue | LValue "++" | LValue "--" | "--" LValue
expr:
  | i=INT_VALUE
    { Ast.ILiteral(i) $$ $loc }
  | f=FLOAT_VALUE
    { Ast.FLiteral(f) $$ $loc }
  | c=CHAR_VALUE
    { Ast.CLiteral(c) $$ $loc }
  | b=BOOL_VALUE
    { Ast.BLiteral(b) $$ $loc }
  | "(" e=expr ")"
    { e }
  | "&" lv=l_value
    { Ast.Address(lv) $$ $loc }
  | lv=l_value "=" e=expr
    { Ast.Assign(lv, e) $$ $loc }

    // Assignment binary operators
  | lv=l_value "+=" e=expr
    { Ast.AssignBinOp(lv, Ast.Add, e) $$ $loc }
  | lv=l_value "-=" e=expr
    { Ast.AssignBinOp(lv, Ast.Sub, e) $$ $loc }
  | lv=l_value "*=" e=expr
    { Ast.AssignBinOp(lv, Ast.Mult, e) $$ $loc }
  | lv=l_value "/=" e=expr
    { Ast.AssignBinOp(lv, Ast.Div, e) $$ $loc }
  | lv=l_value "%=" e=expr
    { Ast.AssignBinOp(lv, Ast.Mod, e) $$ $loc }

  | "!" e=expr
    { Ast.UnaryOp(Ast.Not, e) $$ $loc }
  | id=ID "(" e =separated_list(COMMA, expr) ")"
    { Ast.Call(None, id, e) $$ $loc }
  | lv = l_value
    { Ast.LV(lv) $$ $loc }
  | "-" e=expr %prec NEG
    { Ast.UnaryOp(Ast.Neg, e) $$ $loc }
  | e1=expr bo=bin_op e2=expr
    { Ast.BinaryOp(bo, e1, e2) $$ $loc }
  
  // Increment and decrement
  | "++" lv=l_value
    { Ast.IncDec(lv, Ast.Inc, Ast.Pre) $$ $loc }
  | lv=l_value "++"
    { Ast.IncDec(lv, Ast.Inc, Ast.Post) $$ $loc }
  | "--" e=expr
    { 
      match e.node with
      | Ast.LV(lv) -> Ast.IncDec(lv, Ast.Dec, Ast.Pre) $$ $loc
      | _ -> 
        let unary_op = Ast.UnaryOp(Ast.Neg, e) $$ $loc in
        Ast.UnaryOp(Ast.Neg, unary_op) $$ $loc}
  | lv=l_value "--"
    { Ast.IncDec(lv, Ast.Dec, Ast.Post) $$ $loc }
;


// 16 - LValue ::= ID | ID "[" Expr "]"
l_value:
  | id=ID
    { Ast.AccVar(None, id) $$ $loc  }
  | id=ID e=delimited("[", expr, "]")
    { 
      let l_node = Ast.AccVar(None, id) $$ $loc in
      Ast.AccIndex(l_node, e) $$ $loc  
    }
;


// 17 - BinOp ::= "+" | "-" | "*" | "%" | "/" | "&&" | "||" | "<" | ">" | "<=" | ">=" | "==" | "!="
%inline bin_op:
  | "+"   { Ast.Add     }
  | "-"   { Ast.Sub     }
  | "*"   { Ast.Mult    }
  | "/"   { Ast.Div     }
  | "%"   { Ast.Mod     }
  | "=="  { Ast.Equal   }
  | "!="  { Ast.Neq     }
  | "<"   { Ast.Less    }
  | "<="  { Ast.Leq     }
  | ">"   { Ast.Greater }
  | ">="  { Ast.Geq     }
  | "&&"  { Ast.And     }
  | "||"  { Ast.Or      }
;
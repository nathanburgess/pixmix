(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or 
    | BitAnd | BitOr | BitXor | BitLeft | BitLeftAssn | BitRight | BitRightAssn

type uop = Neg | Not | Incr | Decr | BitNeg

(* Make sure Array of typ does not allow for array of array of array *)
(* Cange name from typ to something less weird *)
type typ = Num | Int | Bool | Void | String | Char | Object | Array of typ | Image | Pixel | Color

type bind = typ * string

type expr =
      Literal       of int (* Change this to Number *)
    | BoolLit       of bool
    | StringLit     of string
    | Id            of string
    | Object        of string
    | Binop         of expr * op * expr
    | ArrayCreate   of typ * expr list
    | Arrop         of string * expr
    | ObjLit        of string * string
    | ObjCall       of string * string * expr list
    | Unop          of uop * expr
    | Assign        of string * expr
    | Call          of string * expr list
    | Noexpr

(* stmt most likely ready *)
type stmt =
      Block         of stmt list
    | Expr          of expr
    | Return        of expr
    | If            of expr * stmt * stmt
    | For           of expr * expr * expr * stmt
    | While         of expr * stmt

type func_decl = {
    typ             : typ;
    fname           : string;
    formals         : bind list;
    locals          : bind list;
    body            : stmt list;
}

type objexpr  = {
    oname           : string;
    olocals         : bind list;
    omethods        : func_decl list;
}

type globals = {
    variables       : bind list;
    objects         : objexpr list;
    statements      : stmt list;
    functions       : func_decl list;
}

type program = Program of globals

(* Pretty-printing functions *)
let string_of_op = function
      Add           -> "+"
    | Sub           -> "-"
    | Mult          -> "*"
    | Div           -> "/"
    | Equal         -> "=="
    | Neq           -> "!="
    | Less          -> "<"
    | Leq           -> "<="
    | Greater       -> ">"
    | Geq           -> ">="
    | And           -> "&&"
    | Or            -> "||"
    | BitAnd        -> "&"
    | BitOr         -> "|"
    | BitXor        -> "^"
    | BitLeft       -> "<<"
    | BitLeftAssn   -> "<<="
    | BitRight      -> ">>"
    | BitRightAssn  -> ">>="
    | Mod           -> "%" (* make sure this pattern matching is exhaustive *)

let string_of_uop = function
      Neg           -> "-"
    | Not           -> "!"
    | Incr          -> "++"
    | Decr          -> "--"
    | BitNeg        -> "~"

(* The carrot "^" concatenates strings! *)
let rec string_of_expr = function
      Literal(l) -> string_of_int l
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    | StringLit(s) -> s
    | Id(s) -> s
    | Object(s) -> s
    | Binop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
    | ArrayCreate(typ, expressions) -> 
        let rec string_list exprs = match expressions with
            [] -> ""
            | [head] -> "[" ^ (string_of_typ typ) ^ ", " ^
            (string_of_expr head) ^ "]"
            | head :: tail -> "[" ^ (string_list tail) ^ ", " ^ (string_of_expr head) ^ "]"
                in
                string_list expressions
    | Arrop(s, e) ->
        s ^ "[" ^ string_of_expr e ^ "]"
    | ObjLit
    | ObjCall
    | Unop(o, e) -> string_of_uop o ^ string_of_expr e
    | Assign(v, e) -> v ^ " = " ^ string_of_expr e
    | Call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Noexpr -> ""

let rec string_of_stmt = function
      Block(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
    | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                        string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
    | For(e1, e2, e3, s) ->
        "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
        string_of_expr e3  ^ ") " ^ string_of_stmt s
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
      Int -> "Int"
    | Num -> "num"
    | Bool -> "bool"
    | Void -> "void"
    | String -> "String"
    | Char -> "char"
    | Object -> "Object"
    | Array -> "Array"
    | Image -> "Image"
    | Pixel -> "Pixel"
    | Color -> "Color"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
    string_of_typ fdecl.typ ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.locals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_odecl odecl =
    "Object " ^ odecl.oname ^ " {" ^
    String.concat "" (List.map string_of_vdecl odecl.olocals) ^
    String.concat "" (List.map string_of_fdecl odecl.omethods) ^
    "}\n"

let string_of_globals globals =
    String.concat "" (List.map string_of_vdecl globals.variables) ^ "\n" ^
    String.concat "" (List.map string_of_odecl globals.objects) ^ "\n" ^
    (* String.concat "" (List.map string_of_sdecl globals.statements) ^ "\n" ^ *)
    String.concat "\n" (List.map string_of_fdecl globals.functions)

let string_of_program program = match program with
    Program globals -> string_of_globals globals

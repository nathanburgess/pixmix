(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or 
    | BitAnd | BitOr | BitXor | BitLeft | BitLeftAssn | BitRight | BitRightAssn

type uop = Neg | Not | Incr | Decr | BitNeg

(* Make sure Array of typ does not allow for array of array of array *)
(* Cange name from typ to something less weird *)
(* Object can only be of typ if we add class to typ so need to change this later *)
type varType = Num | Bool | Void | String | Char | Object | Array of varType | Image | Pixel | Color

type bind = varType * string

type expr =
      Literal       of float
    | BoolLit       of bool
    | StringLit     of string
    | Id            of string
    | Binop         of expr * op * expr
    | ArrayCreate   of varType * expr list
    | Arrop         of string * expr
    | ObjLit        of string * string
    | ObjCall       of string * string * expr list
    | Unop          of uop * expr
    | DeclAssign    of varType * string * expr
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

type funDecl = {
    returnType      : varType;
    fnName          : string;
    parameters      : bind list;
    fnLocals        : bind list;
    body            : stmt list;
}

type stmnts = {
    statements : stmt list;
}

type objDecl  = {
    objName         : string;
    objLocals       : bind list;
    methods         : funDecl list;
}

type program = {
    variables       : bind list;
    objects         : objDecl list;
    statements      : stmnts list;
    functions       : funDecl list;
}


(*
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

let rec string_of_type = function
      Num           -> "num"
    | Bool          -> "bool"
    | Void          -> "void"
    | String        -> "String"
    | Char          -> "char"
    | Object        -> "Object"
    | Array(t)      -> string_of_type t ^ "[]"
    | Image         -> "Image"
    | Pixel         -> "Pixel"
    | Color         -> "Color"

(* The carrot "^" concatenates strings! *)
let rec string_of_expr = function
      Literal(l) -> string_of_float l
    | BoolLit(true) -> "true"
    | BoolLit(false) -> "false"
    | StringLit(s) -> s
    | Id(s) -> s
    | Binop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
    | ArrayCreate(varType, expressions) -> 
        let rec string_list expressions = match expressions with
            [] -> ""
            | [head] -> "[" ^ (string_of_type varType) ^ ", " ^
            (string_of_expr head) ^ "]"
            | head :: tail -> "[" ^ (string_list tail) ^ ", " ^ (string_of_expr head) ^ "]"
                in
                string_list expressions
    | Arrop(s, e) ->
        s ^ "[" ^ string_of_expr e ^ "]"
    | ObjLit(s1, s2) -> s1 ^ "." ^ s2 
    | ObjCall(s1, s2, e) ->
        s1 ^ "." ^ s2 ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
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

let string_of_vdecl (t, id) = string_of_type t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
    string_of_type fdecl.returnType ^ " " ^
    fdecl.fnName ^ "(" ^ String.concat ", " (List.map snd fdecl.parameters) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.fnLocals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_odecl odecl =
    "Object " ^ odecl.objName ^ " {" ^
    String.concat "" (List.map string_of_vdecl odecl.objLocals) ^
    String.concat "" (List.map string_of_fdecl odecl.methods) ^
    "}\n"

let string_of_program globals =
    String.concat "" (List.map string_of_vdecl globals.variables) ^ "\n" ^
    String.concat "" (List.map string_of_odecl globals.objects) ^ "\n" ^
    (*String.concat "" (List.map string_of_stmt globals.statements) ^ "\n" ^*)
    String.concat "\n" (List.map string_of_fdecl globals.functions)
*)
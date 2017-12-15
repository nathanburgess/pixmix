type binop =

    | Add
    | Sub
    | Mult
    | Div
    | Mod
    | Equal
    | Neq
    | Less
    | Leq
    | Greater
    | Geq
    | And
    | Or

and unop = Neg | Not

and varType =
    | NullType
    | VoidType
    | IntType
    | FloatType
    | StringType
    | BoolType
    | NodeType
    | ArrayType of varType

and formal = Formal        of varType * string

and local  = Local         of varType * string * expr

and expr =
    | Null
    | Noexpr
    | IntLit                of int
    | FloatLit              of float
    | StringLit             of string
    | BoolLit               of bool
    | Node                  of expr
    | Binop                 of expr * binop * expr
    | Unop                  of unop * expr
    | Id                    of string
    | Assign                of string * expr
    | Call                  of string * expr list
    | CallDefault           of expr * string * expr list
    | ArrayCreate           of varType * expr list
    | ArrayAccess           of expr * expr

and stmt =
    | Expr                  of expr
    | Return                of expr
    | For                   of expr * expr * expr * stmt list
    | If                    of expr * stmt list * stmt list
    | While                 of expr * stmt list
    | Variable              of local
    | Function              of funcDecl

and objDecl = {
    objName     :           string;
    objLocals   :           local list;
    objMethods  :           funcDecl list;
}

and funcDecl = {
    returnType  :           varType;
    name        :           string;
    args        :           formal list;
    body        :           stmt list;
}

and program = stmt list

let rec string_of_binop = function
    | Add           -> "+"
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
    | Mod           -> "%"
    (*| BitAnd        -> "&"
    | BitOr         -> "|"
    | BitXor        -> "^"
    | BitLeft       -> "<<"
    | BitLeftAssn   -> "<<="
    | BitRight      -> ">>"
    | BitRightAssn  -> ">>="*)

and string_of_unop = function
    | Neg           -> "-"
    | Not           -> "!"
    (*| Incr          -> "++"
    | Decr          -> "--"
    | BitNeg        -> "~"*)

and string_of_varType = function   
    | IntType -> "int"
    | FloatType -> "float"
    | StringType -> "string"
    | BoolType -> "bool" 
    | NodeType -> "node"
    | NullType -> "null"

and string_of_local = function 
    | Local(t, s, e) -> string_of_expr e

and string_of_expr = function
    | Null -> "null"
    | Noexpr -> ""
    | IntLit i -> string_of_int i
    | FloatLit f -> string_of_float f
    | StringLit s -> "\"" ^ s ^ "\""
    | BoolLit b -> "BoolLit;\n"
    | Node e -> "Node;\n"
    | Binop(e1, op, e2) -> 
        string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
    | Unop(op, e) -> string_of_unop op ^ string_of_expr e
    | Id s -> "Id;\n"
    | Assign(s, e) -> s ^ " = " ^ string_of_expr e ^ ";\n"
    | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
    | CallDefault(e, s, es) -> "CallDefault;\n"

and string_of_statements = function
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> "return " ^ string_of_expr expr
    | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") "
        ^ String.concat "" (List.map string_of_statements s)
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" 
        ^ String.concat "" (List.map string_of_statements s1) 
        ^ "else\n" ^ String.concat "" (List.map string_of_statements s2)
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "" (List.map string_of_statements s)
    | Variable(v) -> "" 
    | Function(f) -> "function;\n"

and string_of_statements2 = function
    | Expr(expr) -> "expression;\n"
    | Return(expr) -> "return;\n"
    | For(e1, e2, e3, s) -> "for loop;\n"
    | If(e, s, l) -> "if statement;\n"
    | While(e, s) -> "while loop;\n"
    | Variable(v) -> "variable;\n"
    | Function(f) -> "function;\n"

and string_of_program stmnts = 
    String.concat "" (List.map string_of_statements stmnts) ^ "\n"
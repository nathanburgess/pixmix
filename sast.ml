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

and formal = Formal         of varType * string

and local  = Local          of varType * string * expr

and expr =
    | Null
    | Noexpr
    | IntLit                of int
    | FloatLit              of float
    | StringLit             of string
    | BoolLit               of bool
    | Node                  of int * expr
    | Binop                 of expr * binop * expr
    | Unop                  of unop * expr
    | Id                    of string
    | Assign                of string * expr
    | ArrayCreate           of expr * list varType
    | ArrayAccess           of expr * expr * varType
    | Call                  of string * expr list
    | CallDefault           of expr * string * expr list

and stmt =
    | Expr                  of expr
    | Return                of expr
    | For                   of expr * expr * expr * stmt list
    | If                    of expr * stmt list * stmt list
    | While                 of expr * stmt list

and funcDecl = {
    returnType  :           varType;
    name        :           string;
    args        :           formal list;
    body        :           stmt list;
    locals      :           formal list;
    parent      :           string;
}

and program = funcDecl list

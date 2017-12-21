type binop =
    | Add         
    | Sub         
    | Mult        
    | Div         
    | Equal       
    | Neq        
    | Leq        
    | LThan
    | GThan  
    | Geq         
    | And         
    | Or          
    | Mod         

and unop =
    | Neg   
    | Not   
    | Incr  
    | Decr  
    | BitNeg

and varType =
    | NullType
    | VoidType
    | IntType
    | NumType
    | StringType
    | BoolType
    | ObjectType
    | ArrayType of varType

and formal = Formal        of varType * string

and local  = Local         of varType * string * expr

and expr =
    | Null
    | Noexpr
    | NumLit                of float
    | IntLit                of int
    | StringLit             of string
    | BoolLit               of bool
    | Binop                 of expr * binop * expr
    | Unop                  of unop * expr
    | Id                    of string
    | Assign                of string * expr
    | Call                  of string * expr list
    | CallObject            of string * string * expr list
    | ObjectAccess          of string * string
    | ArrayCreate           of varType * string * expr
    | ArrayAccess           of expr * expr

and stmt =
    | Expr                  of expr
    | Return                of expr
    | For                   of expr * expr * expr * stmt list
    | If                    of expr * stmt list * stmt list
    | While                 of expr * stmt list
    | Variable              of local
    | Function              of funcDecl
    | Object                of objBody

and objBody = {
    objName     :           string;
    objStmts    :           stmt list;
}

and funcDecl = {
    returnType  :           varType;
    name        :           string;
    args        :           formal list;
    body        :           stmt list;
}

and program = stmt list

let rec stringOfBinop = function
    | Add           -> "+"
    | Sub           -> "-"
    | Mult          -> "*"
    | Div           -> "/"
    | Equal         -> "=="
    | Neq           -> "!="
    | LThan         -> "<"
    | Leq           -> "<="
    | GThan         -> ">"
    | Geq           -> ">="
    | And           -> "&&"
    | Or            -> "||"
    | Mod           -> "%"

and stringOfUnop e = function
    | Neg           -> "-" ^ stringOfExpr e
    | Not           -> "!" ^ stringOfExpr e
    | Incr          -> "++" ^ stringOfExpr e
    | Decr          -> "--" ^ stringOfExpr e
    | BitNeg        -> "~" ^ stringOfExpr e

and stringOfVarType = function  
    | NullType      -> "null" 
    | VoidType      -> "void" 
    | IntType       -> "int"
    | NumType       -> "num"
    | StringType    -> "string"
    | BoolType      -> "bool" 
    | ObjectType    -> "Object"
    | ArrayType(t)  -> "[" ^ stringOfVarType t ^ "]"

and stringOfLocal = function 
    | Local(t, s, e) -> stringOfVarType t ^ " " ^ s ^ " = " ^ stringOfExpr e ^ ";\n"

and string_of_formal = function 
    | Formal(t, s) -> stringOfVarType t ^ " " ^ s

and stringOfExpr = function
    | Null -> "null"
    | Noexpr -> "undefined"
    | NumLit i -> string_of_float i
    | StringLit s -> "\"" ^ String.escaped s ^ "\""
    | BoolLit b -> if b then "true" else "false"
    | Binop(e1, op, e2) -> stringOfExpr e1 ^ " " ^ stringOfBinop op ^ " " ^ stringOfExpr e2
    | Unop(op, e) -> stringOfUnop e op
    | Id s -> s
    | Assign(s, e) -> s ^ " = " ^ stringOfExpr e
    | ArrayCreate(t, n, e) -> "Array " ^ stringOfVarType t ^ " " ^ n ^ " = " ^ " [" ^ stringOfExpr e ^ "]"
    | ArrayAccess(arrCreate, index) -> stringOfExpr arrCreate ^ "[" ^ stringOfExpr index ^ "]"   
    | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map stringOfExpr e) ^ ")"
    | CallObject(o, f, e) -> o ^ "." ^ f ^ "(" ^ String.concat ", " (List.map stringOfExpr e) ^ ")"
    | ObjectAccess(o, v) -> o ^ "." ^ v

and stringOfObject o =
    "Object " ^ o.objName ^ " = {\n"
        ^ String.concat "" (List.map stringOfStatement o.objStmts) ^ "};\n"

and stringOfFunction f =
    stringOfVarType f.returnType ^ " " ^ f.name ^ "(" ^
    String.concat ", " (List.map string_of_formal f.args) ^ ")\n{\n" ^
    String.concat "" (List.map stringOfStatement f.body) ^ "}\n"

and stringOfStatement = function
    | Expr(expr) -> stringOfExpr expr ^ ";\n";
    | Return(expr) -> "return " ^ stringOfExpr expr ^ ";\n"
    | For(e1, e2, e3, s) -> "for (" ^ stringOfExpr e1  ^ " ; " ^ stringOfExpr e2 ^ " ; " ^ stringOfExpr e3  ^ ") "
        ^ String.concat "" (List.map stringOfStatement s)
    | If(e, s1, s2) ->  "if (" ^ stringOfExpr e ^ ")\n" 
        ^ String.concat "" (List.map stringOfStatement s1) 
        ^ "else\n" ^ String.concat "" (List.map stringOfStatement s2)
    | While(e, s) -> "while (" ^ stringOfExpr e ^ ") " ^ String.concat "" (List.map stringOfStatement s)
    | Variable(v) -> stringOfLocal v 
    | Function(f) -> stringOfFunction f
    | Object o -> stringOfObject o

and stringOfProgram stmnts = 
    String.concat "" (List.map stringOfStatement stmnts) ^ "\n"

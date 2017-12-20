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
    | BitAnd      
    | BitOr       
    | BitXor      
    | BitLeft     
    | BitLeftAssn 
    | BitRight    
    | BitRightAssn

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

let rec string_of_binop = function
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
    | BitAnd        -> "&"
    | BitOr         -> "|"
    | BitXor        -> "^"
    | BitLeft       -> "<<"
    | BitLeftAssn   -> "<<="
    | BitRight      -> ">>"
    | BitRightAssn  -> ">>="

and string_of_unop e = function
    | Neg           -> "-" ^ string_of_expr e
    | Not           -> "!" ^ string_of_expr e
    | Incr          -> "++" ^ string_of_expr e
    | Decr          -> "--" ^ string_of_expr e
    | BitNeg        -> "~" ^ string_of_expr e

and string_of_varType = function  
    | NullType      -> "null" 
    | VoidType      -> "void" 
    | IntType       -> "int"
    | NumType       -> "num"
    | StringType    -> "string"
    | BoolType      -> "bool" 
    | ObjectType    -> "Object"
    | ArrayType(t)  -> "[" ^ string_of_varType t ^ "]"

and string_of_local = function 
    | Local(t, s, e) -> string_of_varType t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"

and string_of_formal = function 
    | Formal(t, s) -> string_of_varType t ^ " " ^ s

and string_of_expr = function
    | Null -> "null"
    | Noexpr -> "undefined"
    | NumLit i -> string_of_float i
    | StringLit s -> "\"" ^ String.escaped s ^ "\""
    | BoolLit b -> if b then "true" else "false"
    | Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
    | Unop(op, e) -> string_of_unop e op
    | Id s -> s
    | Assign(s, e) -> s ^ " = " ^ string_of_expr e
    (*| ArrayCreate(typ, exprs) -> 
        let rec string_list exprs = match exprs with
            | [] -> ""
            | [head] -> "[" ^ (string_of_varType typ) ^ ", " ^ 
              (string_of_expr head) ^ "]"
            | head :: tail -> "[" ^ (string_list tail) ^ ", " ^
              (string_of_expr head) ^ "]"
        in
        string_list exprs*)
    | ArrayAccess(arrCreate, index) -> string_of_expr arrCreate ^ "[" ^ string_of_expr index ^ "]"   
    | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
    | CallObject(o, f, e) -> o ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
    | ObjectAccess(o, v) -> o ^ "." ^ v

and string_of_object o =
    "Object " ^ o.objName ^ " = {\n"
        ^ String.concat "" (List.map string_of_statement o.objStmts) ^ "};\n"

and string_of_function f =
    string_of_varType f.returnType ^ " " ^ f.name ^ "(" ^
    String.concat ", " (List.map string_of_formal f.args) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_statement f.body) ^ "}\n"

and string_of_statement = function
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
    | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") "
        ^ String.concat "" (List.map string_of_statement s)
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" 
        ^ String.concat "" (List.map string_of_statement s1) 
        ^ "else\n" ^ String.concat "" (List.map string_of_statement s2)
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "" (List.map string_of_statement s)
    | Variable(v) -> string_of_local v 
    | Function(f) -> string_of_function f
    | Object o -> string_of_object o

and string_of_program stmnts = 
    String.concat "" (List.map string_of_statement stmnts) ^ "\n"

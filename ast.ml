type binop =
    | Add         
    | Sub         
    | Mult        
    | Div         
    | Equal       
    | Neq         
    | Less        
    | Leq         
    | Greater     
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
    | BitAnd        -> "&"
    | BitOr         -> "|"
    | BitXor        -> "^"
    | BitLeft       -> "<<"
    | BitLeftAssn   -> "<<="
    | BitRight      -> ">>"
    | BitRightAssn  -> ">>="

and string_of_unop = function
    | Neg           -> "-"
    | Not           -> "!"
    | Incr          -> "++"
    | Decr          -> "--"
    | BitNeg        -> "~"

and string_of_varType = function   
    | IntType       -> "int"
    | FloatType     -> "float"
    | StringType    -> "string"
    | BoolType      -> "bool" 
    | NodeType      -> "node"
    | NullType      -> "null"
    | ArrayType(t)  -> "[" ^ string_of_varType t ^ "]"

and string_of_local = function 
    | Local(t, s, e) -> string_of_varType t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"

and string_of_formal = function 
    | Formal(t, s) -> string_of_varType t ^ " " ^ s

and string_of_expr = function
    | Null -> "null"
    | Noexpr -> ""
    | IntLit i -> string_of_int i
    | FloatLit f -> string_of_float f
    | StringLit s -> "\"" ^ String.escaped s ^ "\""
    | BoolLit b -> if b then "true" else "false"
    | Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
    | Unop(op, e) -> string_of_unop op ^ string_of_expr e
    | Id s -> s
    | Assign(s, e) -> s ^ " = " ^ string_of_expr e
    | ArrayCreate(typ, exprs) -> 
        let rec string_list exprs = match exprs with
            | [] -> ""
            | [head] -> "[" ^ (string_of_varType typ) ^ ", " ^ 
              (string_of_expr head) ^ "]"
            | head :: tail -> "[" ^ (string_list tail) ^ ", " ^
              (string_of_expr head) ^ "]"
        in
        string_list exprs
    | ArrayAccess(arrCreate, index) -> string_of_expr arrCreate ^ 
        "[" ^ string_of_expr index ^ "]"   
    | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
    | CallDefault(e, s, es) -> "CallDefault;\n"

and string_of_function f =
    string_of_varType f.returnType ^ " " ^ f.name ^ "(" ^
    String.concat ", " (List.map string_of_formal f.args) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_statements f.body) ^ "}\n"

and string_of_statements = function
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> "return " ^ string_of_expr expr
    | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") "
        ^ String.concat "" (List.map string_of_statements s)
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" 
        ^ String.concat "" (List.map string_of_statements s1) 
        ^ "else\n" ^ String.concat "" (List.map string_of_statements s2)
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "" (List.map string_of_statements s)
    | Variable(v) -> string_of_local v 
    | Function(f) -> string_of_function f

and string_of_program stmnts = 
    String.concat "" (List.map string_of_statements stmnts) ^ "\n"
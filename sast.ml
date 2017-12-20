module A = Ast
module StringMap = Map.Make(String)

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
    | ArrayType             of varType

and formal = Formal         of varType * string

and local  = Local          of varType * string * expr

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
    | ArrayCreate           of expr
    | ArrayAccess           of expr * expr
    | Call                  of string * expr list
    | CallObject            of string * string * expr list
    | ObjectAccess          of string * string

and stmt =
    | Expr                  of expr
    | Return                of expr
    | For                   of expr * expr * expr * stmt list
    | If                    of expr * stmt list * stmt list
    | While                 of expr * stmt list
    | Object                of objBody

and objBody = {
    objName     :           string;
    objStmts    :           stmt list;
    objLocals   :           formal list;
}

and funcDecl = {
    returnType  :           varType;
    name        :           string;
    args        :           formal list;
    body        :           stmt list;
    locals      :           formal list;
    parent      :           string;
}

and program = funcDecl list


(* SAST Printing Functions *)

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

and string_of_unop = function
    | Neg           -> "-"
    | Not           -> "!"
    | Incr          -> "++"
    | Decr          -> "--"
    | BitNeg        -> "~"

and string_of_varType = function   
    | NumType -> "num"
    | IntType -> "int"
    | StringType -> "string"
    | BoolType -> "bool" 
    | NullType -> "null"
    | ArrayType(t) -> "array [" ^ string_of_varType t ^ "]"

and string_of_formal (Formal(t,s)) = string_of_varType t ^ " " ^ s ^ ";\n"

and string_of_local (Local(t,s,e)) = string_of_varType t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"

and string_of_expr = function
    | Null -> "null"
    | Noexpr -> "noexpr"
    | NumLit f -> string_of_float f
    | StringLit s -> "\"" ^ String.escaped s ^ "\""
    | BoolLit b -> if b then "true" else "false"
    | Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
    | Unop(op, e) -> string_of_unop op ^ string_of_expr e
    | Id s -> s
    | Assign(s, e) -> s ^ " = " ^ string_of_expr e
    (*| ArrayCreate(typ, expressions) ->  
        let rec string_list exprs = match exprs with
            | [] -> ""
            | [head] -> "[" ^ (string_of_varType typ) ^ ", " ^ 
              (string_of_expr head) ^ "]"
            | head :: tail -> "[" ^ (string_list tail) ^ ", " ^
              (string_of_expr head) ^ "]"
        in
        string_list exprs*)
    | ArrayCreate(e) -> "[" ^ string_of_expr e ^ "]" 
    | ArrayAccess(arrCreate, index) -> string_of_expr arrCreate 
        ^ "[" ^ string_of_expr index ^ "]"   
    | Call(f, e) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
    | CallObject(o, f, e) -> o ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"

and string_of_statement = function
    | Expr(expr) -> string_of_expr expr ^ ";\n";
    | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
    | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") "
        ^ String.concat "" (List.map string_of_statement s)
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" 
        ^ String.concat "" (List.map string_of_statement s1) 
        ^ "else\n" ^ String.concat "" (List.map string_of_statement s2)
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "" (List.map string_of_statement s)
    | Object o -> "Object " ^ o.objName ^ " = {\n"
        ^ String.concat "" (List.map string_of_formal o.objLocals) ^ "};\n"

and string_of_function f =
    string_of_varType f.returnType ^ " " ^ f.name ^ "(" ^
    String.concat ", " (List.map string_of_formal f.args) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_formal f.locals) ^
    String.concat "" (List.map string_of_statement f.body) ^ "}\n"

and string_of_program funcs = 
    "\n\n" ^ String.concat "" (List.map string_of_function funcs) ^ "\n"

let convertBinOp = function
    | A.Add                 -> Add
    | A.Sub                 -> Sub
    | A.Mult                -> Mult
    | A.Div                 -> Div
    | A.Mod                 -> Mod
    | A.Equal               -> Equal
    | A.Neq                 -> Neq
    | A.LThan               -> LThan
    | A.Leq                 -> Leq
    | A.GThan               -> GThan
    | A.Geq                 -> Geq
    | A.And                 -> And
    | A.Or                  -> Or
    
let convertUnOp = function 
    | A.Neg -> Neg 
    | A.Not -> Not
        
let convertVarType = function
    | A.NullType            -> NullType
    | A.VoidType            -> VoidType
    | A.IntType             -> IntType
    | A.NumType             -> NumType
    | A.StringType          -> StringType
    | A.BoolType            -> BoolType
    
let rec getName map aux curName =
    if StringMap.mem curName map
    then (let aux = (StringMap.find curName map) ^ "." ^ aux in 
        getName map aux (StringMap.find curName map))
    else aux
    
let rec convertExpr map = function
    | A.Null                    -> Null
    | A.Noexpr                  -> Noexpr
    | A.NumLit a                -> NumLit a
    | A.StringLit a             -> StringLit a
    | A.BoolLit a               -> BoolLit a
    | A.Binop (a, b, c)         -> Binop ((convertExpr map a), (convertBinOp b), (convertExpr map c))
    | A.Unop (a, b)             -> Unop ((convertUnOp a), (convertExpr map b))
    | A.Id a                    -> Id a
    | A.Assign (a, b)           -> Assign (a, (convertExpr map b))
    | A.Call (a, b)             -> Call ((getName map a a), (convertExprs map b))
    | A.CallObject (a, b, c)    -> CallObject ((getName map a a), (getName map b b), (convertExprs map c))
    | A.ObjectAccess(a, b)      -> ObjectAccess ((getName map a a), (getName map b b))

and convertExprs map = function
    | [] -> []
    | [ x ] -> [ convertExpr map x ]
    | (_ as l) -> List.map (convertExpr map) l
    
let convertFormal = function 
    | A.Formal (v, s) -> Formal ((convertVarType v), s)
    
let buildFormals = function
    | [] -> []
    | [ x ] -> [ convertFormal x ]
    | (_ as l) -> List.map convertFormal l

let rec getFunctionBodyA = function
    | [] -> []
    | A.Function x :: tl -> getFunctionBodyA tl
    | ((_ as x)) :: tl -> x :: (getFunctionBodyA tl)
    
let rec convertStatement map = function
    | A.Expr a -> Expr (convertExpr map a)
    | A.Return a -> Return (convertExpr map a)
    | A.For (e1, e2, e3, stls) ->
        For ((convertExpr map e1), (convertExpr map e2), (convertExpr map e3), (List.map (convertStatement map) stls))
    | A.If (e, stls1, stls2) ->
        If ((convertExpr map e), (List.map (convertStatement map) stls1), (List.map (convertStatement map) stls2))
    | A.While (e, stls) -> While ((convertExpr map e), (List.map (convertStatement map) stls))
    | _ -> Expr Noexpr    
    
let rec getFunctionLocals = function
    | [] -> []
    | A.Variable (A.Local (t, n, _)) :: tl -> (Formal ((convertVarType t), n)) :: (getFunctionLocals tl)
    | _ :: tl -> getFunctionLocals tl

let rec getFunctionBodyS map = function
    | [] -> []
    | A.Variable (A.Local (_, name, v)) :: tl when v <> A.Noexpr -> (Expr (Assign (name, (convertExpr map v)))) :: (getFunctionBodyS map tl)
    | A.Variable (A.Local (_, _, v)) :: tl when v = A.Noexpr -> getFunctionBodyS map tl
    | ((_ as x)) :: tl -> (convertStatement map x) :: (getFunctionBodyS map tl)
    
let rec getFunctionsA = function
    | [] -> []
    | ((A.Function _ as x)) :: tl -> x :: (getFunctionsA tl)
    | _ :: tl -> getFunctionsA tl

let rec mapper parent map = function
    | [] -> map
    | A.Function { A.name = n; _ } :: tl -> 
        mapper parent (StringMap.add n parent map) tl
    | _ -> map

let buildFunctionBody map = function
    | A.Function { A.name = n; A.body = b; _ } -> 
        let curr = getFunctionsA b in
        let map = mapper n map curr in (curr, map)
    | _ -> ([], map)

let buildMethodBody map parent = function
    | A.Function { A.name = n; A.body = b; _ } -> 
        let curr = getFunctionsA b in
        let map = mapper parent map curr in (curr, map)
    | _ -> ([], map)

let rec convertFunctionList map = function
    | [] -> []
    | A.Function { A.returnType = r; A.name = n; A.args = a; A.body = b } :: tl -> 
        {
            returnType = convertVarType r;
            name = getName map n n;
            args = buildFormals a;
            body = getFunctionBodyS map b;
            locals = getFunctionLocals b;
            parent = if n = "main" then "main" else getName map (StringMap.find n map) (StringMap.find n map);
        } :: (convertFunctionList map tl)
    | A.Object o :: tl ->
        convertFunctionList map tl
    | _ :: tl -> convertFunctionList map tl

let rec buildFunction map result = function
    | [] -> (List.rev result, map)
    | (A.Function { A.returnType = r; A.name = n; A.args = args; A.body = b } as a) :: tl ->
        let result1 = buildFunctionBody map a in
        let latterlist = tl @ (fst result1) in
        let map = snd result1 in 
        let addedFunc = A.Function {
            A.returnType = r;
            A.name = n;
            A.args = args;
            A.body = getFunctionBodyA b;
        } in
        let result = result @ [ addedFunc ] in buildFunction map result latterlist
    | _ -> ([], map)

(*
 * Convert all objects to functions instead
 *)
let rec convertObjects = function
    | [] -> []
    | A.Object o :: tl -> 
        let func = A.Function {
            A.returnType = A.NumType; 
            A.name = o.objName; 
            A.args = [];
            A.body = convertObjects o.objStmts;
        } in 
        func :: (convertObjects tl)
    | ((_ as x)) :: tl -> x :: (convertObjects tl)

let createMain stmts = A.Function 
    { 
        A.returnType = A.IntType; 
        A.name = "main"; 
        A.args = [];
        A.body = convertObjects stmts;
    }

let convert stmts =
    let main = createMain stmts 
    and funcMap = StringMap.empty 
    and resultMap = [] in
        let finalList = buildFunction funcMap [] [ main ] in
            convertFunctionList (snd finalList) (fst finalList)
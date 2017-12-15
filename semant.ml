open Sast
    
open Printf
    
module StringMap = Map.Make(String)
    
let string_of_type =
    function
    | IntType -> "int"
    | FloatType -> "float"
    | StringType -> "string"
    | BoolType -> "bool"
    | NodeType -> "node"
    | NullType -> "null"
    
let string_of_op =
    function
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Equal -> "=="
    | Neq -> "!="
    | Less -> "<"
    | Leq -> "<="
    | Greater -> ">"
    | Geq -> ">="
    | And -> "and"
    | Or -> "or"
    
let string_of_uop = function | Neg -> "-" | Not -> "not"
    
let rec string_of_expr =
    function
    | Null -> "null"
    | Noexpr -> ""
    | IntLit l -> string_of_int l
    | FloatLit l -> string_of_float l
    | StringLit l -> l
    | BoolLit b -> if b then "true" else "false"
    | Node (_, e) -> "node(" ^ ((string_of_expr e) ^ ")")
    | Binop (e1, o, e2) -> (string_of_expr e1) ^
        (" " ^ ((string_of_op o) ^ (" " ^ (string_of_expr e2))))
    | Unop (o, e) -> (string_of_uop o) ^ (" " ^ (string_of_expr e))
    | Id s -> s
    | Assign (v, e) -> v ^ (" = " ^ (string_of_expr e))
    | Call (n, _) -> "function call " ^ n
    
exception SemanticError of string
    
(* error message functions *)
let undeclared_function_error name =
    let msg = sprintf "undeclared function %s" name
    in raise (SemanticError msg)
    
let duplicate_formal_decl_error func name =
    let msg = sprintf "duplicate formal %s in %s" name func.name
    in raise (SemanticError msg)
    
let duplicate_local_decl_error func name =
    let msg = sprintf "duplicate local %s in %s" name func.name
    in raise (SemanticError msg)
    
let undeclared_identifier_error name =
    let msg = sprintf "undeclared identifier %s" name
    in raise (SemanticError msg)
    
let illegal_assignment_error lvaluet rvaluet ex =
    let msg = sprintf "illegal assignment %s = %s in %s" lvaluet rvaluet ex
    in raise (SemanticError msg)
    
let illegal_binary_operation_error typ1 typ2 op ex =
    let msg = sprintf "illegal binary operator %s %s %s in %s" typ1 op typ2 ex
    in raise (SemanticError msg)
    
let illegal_unary_operation_error typ op ex =
    let msg = sprintf "illegal unary operator %s %s in %s" op typ ex
    in raise (SemanticError msg)
    
let unmatched_func_arg_len_error name =
    let msg = sprintf "args length not match in function call: %s" name
    in raise (SemanticError msg)
    
let incompatible_func_arg_type_error typ1 typ2 =
    let msg =
        sprintf "incompatible argument type %s, but %s is expected" typ1 typ2
    in raise (SemanticError msg)
    
let invalid_expr_after_return_error _ =
    let msg = sprintf "nothing may follow a return"
    in raise (SemanticError msg)
    
let redefine_print_func_error _ =
    let msg = sprintf "function print may not be defined"
    in raise (SemanticError msg)
    
let duplicate_func_error name =
    let msg = sprintf "duplicate function declaration: %s" name
    in raise (SemanticError msg)
    
let unsupport_operation_error typ name =
    let msg = sprintf "unsupport operation on type %s: %s" typ name
    in raise (SemanticError msg)
    
let wrong_func_return_type_error typ1 typ2 =
    let msg = sprintf "wrong function return type: %s, expect %s" typ1 typ2
    in raise (SemanticError msg)
    
(*
    Check if something is of type Array
*)
let rec isArrayType t = match t with | ArrayType _ -> true | _ -> false
and getArrayNesting t = match t with | ArrayType inner -> 1 + (getArrayNesting inner) | _ -> 1
    
let checkReturnType func typ =
    let lvaluet = func.returnType
    and rvaluet = typ in
    match lvaluet with
    | FloatType when rvaluet = IntType -> ()
    | StringType when rvaluet = NullType -> ()
    | NodeType when rvaluet = NullType -> ()
    | _ -> if lvaluet == rvaluet then ()
        else wrong_func_return_type_error (string_of_type rvaluet) (string_of_type lvaluet)
    
let getFunctionObject name func_map =
    try StringMap.find name func_map
    with | Not_found -> undeclared_function_error name
    
let reportDuplicateVar exceptf list =
    let rec helper =
        function
        | n1 :: n2 :: _ when n1 = n2 -> exceptf n1
        | _ :: t -> helper t
        | [] -> ()
    in helper (List.sort compare list)
    
(* check function *)
let check_function func_map func = (* check duplicate formals *)
    let args = List.map (function | Formal (_, n) -> n) func.args in
        (reportDuplicateVar (duplicate_formal_decl_error func) args;  

    let locals = List.map (function | Formal (_, n) -> n) func.locals in
         (reportDuplicateVar (duplicate_local_decl_error func) locals;

    let rec type_of_identifier func s =
        let symbols = List.fold_left (fun m -> function | Formal (t, n) -> StringMap.add n t m)
            StringMap.empty (func.args @ func.locals) in
        try StringMap.find s symbols with
        | Not_found ->
            if func.name = "main" 
            then undeclared_identifier_error s
            else type_of_identifier (StringMap.find func.parent func_map) s 
    in

    let check_assign lvaluet rvaluet ex =
        match lvaluet with
        | FloatType when rvaluet = IntType -> lvaluet
        | StringType when rvaluet = NullType -> lvaluet
        | NodeType when rvaluet = NullType -> lvaluet
        | _ -> if lvaluet == rvaluet
            then lvaluet
            else illegal_assignment_error (string_of_type lvaluet) (string_of_type rvaluet) (string_of_expr ex) in

    let rec expr =
        function
        | IntLit _      -> IntType
        | FloatLit _    -> FloatType
        | Null          -> NullType
        | StringLit _   -> StringType
        | BoolLit _     -> BoolType
        | Node (_, _)   -> NodeType
        | (Binop (e1, op, e2) as e) -> let t1 = expr e1 and t2 = expr e2
            in (match op with
                | Add | Sub | Mult | Div when (t1 = IntType) && (t2 = IntType) -> IntType
                | Add | Sub | Mult | Div when (t1 = FloatType) && (t2 = FloatType) -> FloatType
                | Add | Sub | Mult | Div when (t1 = IntType) && (t2 = FloatType) -> FloatType
                | Add | Sub | Mult | Div when (t1 = FloatType) && (t2 = IntType) -> FloatType
                | Equal | Neq when t1 = t2 -> BoolType
                | Less | Leq | Greater | Geq 
                    when ((t1 = IntType) || (t1 = FloatType)) && ((t2 = IntType) || (t2 = FloatType)) -> BoolType
                | And | Or when (t1 = BoolType) && (t2 = BoolType) -> BoolType
                | Mod when (t1 = IntType) && (t2 = IntType) -> IntType
                | _ -> illegal_binary_operation_error (string_of_type t1) (string_of_type t2) (string_of_op op) (string_of_expr e))
        | (Unop (op, e) as ex) -> let t = expr e in (match op with
            | Neg when t = IntType -> IntType
            | Neg when t = FloatType -> FloatType
            | Not when t = BoolType -> BoolType
            | _ -> illegal_unary_operation_error (string_of_type t) (string_of_uop op) (string_of_expr ex))
        | Id s -> type_of_identifier func s
        | (Assign (var, e) as ex) -> let lt = type_of_identifier func var and rt = expr e in
            check_assign lt rt ex
        | Noexpr -> VoidType
        | Call (n, args) -> let func_obj = getFunctionObject n func_map in
            let checkFunctionCall func args =
                let check_args_length l_arg r_arg =
                    if (List.length l_arg) = (List.length r_arg)
                    then ()
                    else unmatched_func_arg_len_error func.name in
                (if List.mem func.name [ "printb"; "print"; "printf"; "string"; "float"; "int"; "bool" ] then ()
                    else check_args_length func.args args;
                    let check_args_type l_arg r_arg =
                        List.iter2 (function 
                            | Formal (t, _) -> (fun r -> let r_typ = expr r in
                                if t = r_typ then ()
                                else incompatible_func_arg_type_error (string_of_type r_typ) (string_of_type t)))
                        l_arg r_arg in
                    if List.mem func.name [ "printb"; "print"; "printf"; "string"; "float"; "int"; "bool" ] then ()
                    else check_args_type func.args args)
                in
            (ignore (checkFunctionCall func_obj args);
        func_obj.returnType) 
    in

    let rec stmt = function
        | Expr e    -> ignore (expr e)
        | Return e  -> ignore (checkReturnType func (expr e))
        | For (e1, e2, e3, stls) -> 
            (
                ignore (expr e1);
                ignore (expr e2);
                ignore (expr e3);
                ignore (stmt_list stls)
            )
        | If (e, stls1, stls2) -> (ignore e; ignore (stmt_list stls1); ignore (stmt_list stls2))
        | While (e, stls) -> (ignore e; ignore (stmt_list stls)) 

    and stmt_list = function
        | Return _ :: ss when ss <> [] -> invalid_expr_after_return_error ss
        | s :: ss -> (stmt s; stmt_list ss)
        | [] -> ()
    in stmt_list func.body))
    
let check program =
    let end_with s1 s2 =
        let len1 = String.length s1
        and len2 = String.length s2 in
            if len1 < len2 then false
            else
                (let last = String.sub s1 (len1 - len2) len2 
            in 
        if last = s2 then true else false)
    in
    (if List.mem true (List.map (fun f -> end_with f.name "print") program)
    then redefine_print_func_error "_"
    else ();
    let m = StringMap.empty in
        (ignore (List.map (fun f -> 
            if StringMap.mem f.name m
            then duplicate_func_error f.name
            else StringMap.add f.name true m) program);
    let built_in_funcs =
        let funcs =
            [ ("print", {
                returnType = VoidType;
                name = "print";
                args = [ Formal (StringType, "x") ];
                locals = [];
                body = [];
                parent = "main";
            });
            ("printf", {
                returnType = VoidType;
                name = "printf";
                args = [ Formal (StringType, "x") ];
                locals = [];
                body = [];
                parent = "main";
            }) ] in
        let add_func funcs m =
            List.fold_left (fun m (n, func) -> StringMap.add n func m) m funcs in
            add_func funcs StringMap.empty in
        let func_map = List.fold_left (fun m f -> StringMap.add f.name f m) built_in_funcs program in
        let check_function_wrapper func m = func m in 
            List.iter (check_function_wrapper check_function func_map) program))

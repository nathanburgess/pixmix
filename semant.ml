open Sast

open Printf

module StringMap = Map.Make(String)

let stringOfContext =
    function
        | IntType -> "int"
        | NumType -> "num"
        | StringType -> "string"
        | BoolType -> "bool"
        | NullType -> "null"

let stringOfOp =
    function
        | Add -> "+"
        | Sub -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Mod -> "%"
        | Equal -> "=="
        | Neq -> "!="
        | Leq -> "<="
        | LThan -> "<"
        | GThan -> ">"
        | Geq -> ">="
        | And -> "and"
        | Or -> "or"

let stringOfUop = function | Neg -> "-" | Not -> "not"

let rec stringOfExpr =
    function
        | Null -> "null"
        | Noexpr -> ""
        | IntLit l -> string_of_int l
        | NumLit l -> string_of_float l
        | StringLit l -> l
        | BoolLit b -> if b then "true" else "false"
        | Binop (e1, o, e2) -> (stringOfExpr e1) ^
            (" " ^ ((stringOfOp o) ^ (" " ^ (stringOfExpr e2))))
        | Unop (o, e) -> (stringOfUop o) ^ (" " ^ (stringOfExpr e))
        | Id s -> s
        | Assign (v, e) -> v ^ (" = " ^ (stringOfExpr e))
        | Call (n, _) -> "function call " ^ n

exception SemanticError of string

(* error message functions *)
let undeclaredFunctionError name =
    let msg = sprintf "undeclared function %s" name
    in raise (SemanticError msg)

let duplicateFormalDeclError func name =
    let msg = sprintf "duplicate formal %s in %s" name func.name
    in raise (SemanticError msg)

let duplicateLocalDeclError func name =
    let msg = sprintf "duplicate local %s in %s" name func.name
    in raise (SemanticError msg)

let undeclaredIdentifierError name =
    let msg = sprintf "undeclared identifier %s" name
    in raise (SemanticError msg)

let illegalAssignmentError lvaluet rvaluet ex =
    let msg = sprintf "illegal assignment %s = %s in %s" lvaluet rvaluet ex
    in raise (SemanticError msg)

let illegalBinaryOperationError typ1 typ2 op ex =
    let msg = sprintf "illegal binary operator %s %s %s in %s" typ1 op typ2 ex
    in raise (SemanticError msg)

let illegalArrayAccessError typ1 typ2 ex =
    let msg = sprintf "illegal array access %s %s in %s" typ1 typ2 ex
    in raise (SemanticError msg)

let illegalUnaryOperationError typ op ex =
    let msg = sprintf "illegal unary operator %s %s in %s" op typ ex
    in raise (SemanticError msg)

let unmatchedFuncArgLenError name =
    let msg = sprintf "args length not match in function call: %s" name
    in raise (SemanticError msg)

let incompatibleFuncArgTypeError typ1 typ2 =
    let msg =
        sprintf "incompatible argument type %s, but %s is expected" typ1 typ2
    in raise (SemanticError msg)

let invalidExprAfterReturnError _ =
    let msg = sprintf "nothing may follow a return"
    in raise (SemanticError msg)

let redifinePrintFuncError _ =
    let msg = sprintf "function print may not be defined"
    in raise (SemanticError msg)

let duplicateFuncError name =
    let msg = sprintf "duplicate function declaration: %s" name
    in raise (SemanticError msg)

let unsupportedOperationError typ name =
    let msg = sprintf "unsupport operation on type %s: %s" typ name
    in raise (SemanticError msg)

let returnTypeMisMatchError typ1 typ2 =
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
            | NumType when rvaluet = NumType -> ()
            | StringType when rvaluet = NullType -> ()
            | _ -> if lvaluet == rvaluet then ()
                else returnTypeMisMatchError (stringOfContext rvaluet) (stringOfContext lvaluet)

let getFunctionObject name func_map =
    try StringMap.find name func_map
    with | Not_found -> undeclaredFunctionError name

let reportDuplicateVar exceptf list =
    let rec helper =
        function
            | n1 :: n2 :: _ when n1 = n2 -> exceptf n1
            | _ :: t -> helper t
            | [] -> ()
    in helper (List.sort compare list)

let checkFunction func_map func =
    let args = List.map (function | Formal (_, n) -> n) func.args in
        (reportDuplicateVar (duplicateFormalDeclError func) args;  

        let locals = List.map (function | Formal (_, n) -> n) func.locals in
             (reportDuplicateVar (duplicateLocalDeclError func) locals;

            let rec typeOfIdentifier func s =
                let symbols = List.fold_left (fun m -> function | Formal (t, n) -> StringMap.add n t m)
                    StringMap.empty (func.args @ func.locals) in
                    try StringMap.find s symbols with
                        | Not_found ->
                            if func.name = "main" 
                            then undeclaredIdentifierError s
                            else typeOfIdentifier (StringMap.find func.parent func_map) s 
            in

            let checkAssign lvaluet rvaluet ex = match lvaluet with
                | NumType when rvaluet = NumType -> lvaluet
                | StringType when rvaluet = NullType -> lvaluet
                | _ -> if lvaluet == rvaluet
                    then lvaluet
                    else illegalAssignmentError (stringOfContext lvaluet) (stringOfContext rvaluet) (stringOfExpr ex) in

            let rec expr = function
                | IntLit _      -> IntType
                | NumLit _      -> NumType
                | Null          -> NullType
                | StringLit _   -> StringType
                | BoolLit _     -> BoolType
                | (Binop (e1, op, e2) as e) -> let t1 = expr e1 and t2 = expr e2 in (match op with
                    | Add | Sub | Mult | Div when (t1 = IntType) && (t2 = IntType) -> IntType
                    | Add | Sub | Mult | Div when (t1 = NumType) && (t2 = NumType) -> NumType
                    | Add | Sub | Mult | Div when (t1 = IntType) && (t2 = NumType) -> NumType
                    | Add | Sub | Mult | Div when (t1 = NumType) && (t2 = IntType) -> NumType
                    | Equal | Neq when t1 = t2 -> BoolType
                    | LThan | Leq | GThan | Geq  when ((t1 = IntType) || (t1 = NumType)) && ((t2 = IntType) || (t2 = NumType)) -> BoolType
                    | And | Or when (t1 = BoolType) && (t2 = BoolType) -> BoolType
                    | Mod when (t1 = IntType) && (t2 = IntType) -> IntType
                    | _ -> illegalBinaryOperationError (stringOfContext t1) (stringOfContext t2) (stringOfOp op) (stringOfExpr e))
                | (Unop (op, e) as ex) -> let t = expr e in (match op with
                    | Neg when t = IntType -> IntType
                    | Neg when t = NumType -> NumType
                    | Not when t = BoolType -> BoolType
                    | _ -> illegalUnaryOperationError (stringOfContext t) (stringOfUop op) (stringOfExpr ex))
                | Id s -> typeOfIdentifier func s
                | (Assign (var, e) as ex) -> let lt = typeOfIdentifier func var and rt = expr e in checkAssign lt rt ex
                | ArrayCreate(_, _, expr1) -> let e_type = expr expr1 in
                    if e_type != IntType
                    then (raise(Failure("Array dimensions must be type int")))
                    else IntType
                | (ArrayAccess(expr1, expr2) as ex) -> let e_type = expr expr1 and e_num = expr expr2 in
                    if (e_num != IntType) 
                        then illegalArrayAccessError (stringOfContext e_type) (stringOfContext e_num) (stringOfExpr ex)
                    else
                        (match e_type with (* add object to this when ready *)
                            | IntType
                            | NumType
                            | StringType
                            | BoolType
                            | _ -> illegalArrayAccessError (stringOfContext e_type) (stringOfContext e_num) (stringOfExpr ex))
                | Noexpr -> VoidType
                | Call (n, args) -> let func_obj = getFunctionObject n func_map in
                    let checkFunctionCall func args =
                        let check_args_length l_arg r_arg =
                            if (List.length l_arg) = (List.length r_arg)
                            then ()
                            else unmatchedFuncArgLenError func.name in
                        (if List.mem func.name [ "printb"; "print"; "printf"; "string"; "float"; "int"; "bool" ] then ()
                        else check_args_length func.args args;
                        let check_args_type l_arg r_arg =
                            List.iter2 (function | Formal (t, _) -> (fun r -> let r_typ = expr r in
                                if t = r_typ 
                                then ()
                                else incompatibleFuncArgTypeError (stringOfContext r_typ) (stringOfContext t)))
                            l_arg r_arg in
                                if List.mem func.name [ "printb"; "print"; "printf"; "string"; "float"; "int"; "bool" ] 
                                then ()
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
                  | Object o -> (ignore o.objName; ignore (stmt_list o.objStmts))

              and stmt_list = function
                  | Return _ :: ss when ss <> [] -> invalidExprAfterReturnError ss
                  | s :: ss -> (stmt s; stmt_list ss)
                  | [] -> ()
              in stmt_list func.body))

let check program =
    let m = StringMap.empty in
        (ignore (List.map (fun f -> 
            if StringMap.mem f.name m
            then duplicateFuncError f.name
            else StringMap.add f.name true m) program));
    let built_in_funcs =
        let funcs = [ ("print", {
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
        })] in
        let add_func funcs m =
            List.fold_left (fun m (n, func) -> StringMap.add n func m) m funcs in
            add_func funcs StringMap.empty in
        let func_map = List.fold_left (fun m f -> StringMap.add f.name f m) built_in_funcs program in
        let checkFunction_wrapper func m = func m in 
            List.iter (checkFunction_wrapper checkFunction func_map) program

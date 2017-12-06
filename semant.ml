(* Semantic checking for the PixMix compiler *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check program = 

    (* Raise an exception if the given list has a duplicate *)
    let report_duplicate exceptf list =
        let rec helper = function
              n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
            | _ :: t -> helper t
            | [] -> ()
        in helper (List.sort compare list)
    in

    (* Raise an exception if a given binding is to a void type *)
    let check_not_void exceptf = function
          (Void, n) -> raise (Failure (exceptf n))
        | _ -> ()
    in

    (* Raise an exception of the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
        if lvaluet == rvaluet then lvaluet else raise err
    in

    (**** Checking Global Variables ****)

    List.iter (check_not_void (fun n -> "illegal void global " ^ n)) program.variables;

    report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd program.variables);

    (**** Checking Functions ****)

    if List.mem "print" (List.map (fun fd -> fd.fnName) program.functions)
    then raise (Failure ("function print may not be defined")) else ();

    report_duplicate (fun n -> "duplicate function " ^ n)
        (List.map (fun fd -> fd.fnName) program.functions);

    (* Function declaration for a named function *)
    let built_in_decls =  
        StringMap.add "print"
        { 
            fnReturnType = Void; 
            fnName = "print"; 
            fnParameters = [(Num, "x")];
            fnLocals = []; 
            fnBody = [] 
        } (StringMap.add "printb"
        { 
            fnReturnType = Void; 
            fnName = "printb"; 
            fnParameters = [(Bool, "x")];
            fnLocals = []; 
            fnBody = [] 
        } (StringMap.singleton "printbig"
        { 
            fnReturnType = Void; 
            fnName = "printbig"; 
            fnParameters = [(Num, "x")];
            fnLocals = []; 
            fnBody = [] 
        }))
    in

    let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fnName fd m)
        built_in_decls program.functions
    in

    let function_decl s = try StringMap.find s function_decls
        with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in

    let check_function func =

        List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
                                            " in " ^ func.fnName)) func.fnParameters;

        report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fnName)
            (List.map snd func.fnParameters);

        List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
                                            " in " ^ func.fnName)) func.fnLocals;

        report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fnName)
            (List.map snd func.fnLocals);

        (* Type of each variable (global, formal, or local *)
        let symbols = List.fold_left (fun m (t, n) -> 
                Printf.printf "; Adding to symbols: %s\n" n;
            StringMap.add n t m)
            StringMap.empty (program.variables @ func.fnParameters @ func.fnLocals )
        in

        let type_of_identifier s =
            try StringMap.find s symbols
            with Not_found -> raise (Failure ("undeclared identifier " ^ s))
        in

        (* Return the type of an expression or throw an exception *)
        let rec expr = function
              Literal _ -> Num
            | BoolLit _ -> Bool
            | StringLit _ -> String
            | Id s -> type_of_identifier s
            | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
                (match op with
                   Add | Sub | Mult | Div when t1 = Num && t2 = Num -> Num
                 | Equal | Neq when t1 = t2 -> Bool
                 | Less | Leq | Greater | Geq when t1 = Num && t2 = Num -> Bool
                 | And | Or when t1 = Bool && t2 = Bool -> Bool
                 | _ -> raise (Failure ("illegal binary operator " ^
                                        string_of_varType t1 ^ " " ^ string_of_op op ^ " " ^
                                        string_of_varType t2 ^ " in " ^ string_of_expr e))
                )
            | Unop(op, e) as ex -> let t = expr e in
                (match op with
                   Neg when t = Num -> Num
                 | Not when t = Bool -> Bool
                 | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
                                        string_of_varType t ^ " in " ^ string_of_expr ex)))
            | Noexpr -> Void
            | Assign(var, e) as ex -> 
                let lt = type_of_identifier var
                and rt = expr e in
                check_assign lt rt (Failure ("illegal assignment " ^ string_of_varType lt ^
                    " = " ^ string_of_varType rt ^ " in " ^ string_of_expr ex))
            | Call(fname, actuals) as call -> let fd = function_decl fname in
                if List.length actuals != List.length fd.fnParameters then
                    raise (Failure ("expecting " ^ string_of_int (List.length fd.fnParameters) ^ " arguments in " ^ string_of_expr call))
                else
                    List.iter2 (fun (ft, _) e -> let et = expr e in
                        ignore (check_assign ft et (Failure ("illegal actual argument found " ^ string_of_varType et ^ " expected " ^ string_of_varType ft ^ " in " ^ string_of_expr e))))
                        fd.fnParameters actuals;
                fd.fnReturnType
            (* | ArrayCreate(t, e) -> 
            | ArrOp(t, e) -> 
            | ObjLit(oid, var) -> 
            | ObjCall(oid, var, e) -> *)
        in

        let check_bool_expr e = if expr e != Bool
            then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
            else () in

        (* Verify a statement or throw an exception *)
        let rec stmt = function
            | Block sl -> let rec check_block = function
                  [Return _ as s] -> stmt s
                | Return _ :: _ -> raise (Failure "nothing may follow a return")
                | Block sl :: ss -> check_block (sl @ ss)
                | s :: ss -> stmt s ; check_block ss
                | [] -> ()
                in check_block sl

            | DeclAssign(t, var, e) as st -> ()

            | Expr e -> ignore (expr e)
            | Return e -> let t = expr e in if t = func.fnReturnType then () else
                    raise (Failure ("return gives " ^ string_of_varType t ^ " expected " ^
                                    string_of_varType func.fnReturnType ^ " in " ^ string_of_expr e))

            | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
            | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                ignore (expr e3); stmt st
            | While(p, s) -> check_bool_expr p; stmt s
        in

        stmt (Block func.fnBody)

    in

    List.iter check_function program.functions

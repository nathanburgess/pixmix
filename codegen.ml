module L = Llvm
module S = Sast

module StringMap = Map.Make(String)

let llctx       = L.global_context ()
and utilsBuffer = L.MemoryBuffer.of_file "lib/utils.bc"
let llm         = Llvm_bitreader.parse_bitcode llctx utilsBuffer

let context     = L.global_context  ()
let the_module  = L.create_module   context "PixMix"

and i32_t       = L.i32_type        context
and f_t         = L.double_type     context
and i8_t        = L.i8_type         context
and i1_t        = L.i1_type         context
and str_t       = L.pointer_type    (L.i8_type context)
and obj_t       = L.pointer_type    (L.i8_type context)
and void_t      = L.void_type       context
and void_ptr_t  = L.pointer_type    (L.i8_type context)

and node_t =
    L.pointer_type
        (match L.type_by_name llm "struct.Node" with
            | None -> raise (Failure "struct.Node doesn't defined.")
            | Some x -> x)

let ltype_of_typ =
    function
        | S.VoidType -> void_t
        | S.IntType -> i32_t
        | S.NumType -> f_t
        | S.BoolType -> i1_t
        | S.StringType -> str_t
        | S.NodeType -> node_t
        | S.ObjectType -> obj_t
        | _ -> raise (Failure "[Error] Type Not Found for ltype_of_typ.")

let lconst_of_typ =
    function
        | S.IntType -> L.const_int i32_t 0
        | S.NumType -> L.const_int i32_t 1
        | S.BoolType -> L.const_int i32_t 2
        | S.StringType -> L.const_int i32_t 3
        | S.NodeType -> L.const_int i32_t 4
        | S.ObjectType -> L.const_int i32_t 5
        | _ -> raise (Failure "[Error] Type Not Found for lconst_of_typ.")

let int_zero = L.const_int i32_t 0
and float_zero = L.const_float f_t 0.
and bool_false = L.const_int i1_t 0
and bool_true = L.const_int i1_t 1
and const_null = L.const_int i32_t 0
and str_null = L.const_null str_t
and node_null = L.const_null node_t
and object_null = L.const_null obj_t

let get_null_value_of_type =
    function
        | S.StringType -> str_null
        | S.NodeType -> node_null
        | S.ObjectType -> object_null
        | _ -> raise (Failure "[Error] Type Not Found for get_null_value_of_type.")

let get_default_value_of_type =
    function
        | (S.IntType as t) -> L.const_int (ltype_of_typ t) 0
        | (S.BoolType as t) -> L.const_int (ltype_of_typ t) 0
        | (S.NumType as t) -> L.const_float (ltype_of_typ t) 0.
        | t -> L.const_null (ltype_of_typ t)

(*
================================================================
    Casting
================================================================
*)
let int_to_float llbuilder v = L.build_sitofp v f_t "tmp" llbuilder

let void_to_int_t = L.function_type i32_t [| L.pointer_type i8_t |]

let void_to_int_f = L.declare_function "VoidtoInt" void_to_int_t the_module

let void_to_int void_ptr llbuilder =
    let actuals = [| void_ptr |]
    in L.build_call void_to_int_f actuals "VoidtoInt" llbuilder

let void_to_float_t = L.function_type f_t [| L.pointer_type i8_t |]

let void_to_float_f = L.declare_function "VoidtoFloat" void_to_float_t the_module

let void_to_float void_ptr llbuilder =
    let actuals = [| void_ptr |]
    in L.build_call void_to_float_f actuals "VoidtoFloat" llbuilder

let void_to_bool_t = L.function_type i1_t [| L.pointer_type i8_t |]

let void_to_bool_f = L.declare_function "VoidtoBool" void_to_bool_t the_module

let void_to_bool void_ptr llbuilder =
    let actuals = [| void_ptr |]
    in L.build_call void_to_bool_f actuals "VoidtoBool" llbuilder

let void_to_string_t = L.function_type str_t [| L.pointer_type i8_t |]

let void_to_string_f = L.declare_function "VoidtoString" void_to_string_t the_module

let void_to_string void_ptr llbuilder =
    let actuals = [| void_ptr |]
    in L.build_call void_to_string_f actuals "VoidtoString" llbuilder

let void_to_node_t = L.function_type node_t [| L.pointer_type i8_t |]

let void_to_node_f = L.declare_function "VoidtoNode" void_to_node_t the_module

let void_to_node void_ptr llbuilder =
    let actuals = [| void_ptr |]
    in L.build_call void_to_node_f actuals "VoidtoNode" llbuilder

let void_start_to_tpy value_void_ptr llbuilder =
    function
        | S.IntType -> void_to_int value_void_ptr llbuilder
        | S.NumType -> void_to_float value_void_ptr llbuilder
        | S.BoolType -> void_to_bool value_void_ptr llbuilder
        | S.StringType -> void_to_string value_void_ptr llbuilder
        | S.NodeType -> void_to_node value_void_ptr llbuilder
        | _ -> raise (Failure "[Error] Unsupported value type.")

(*
 * Built-in function declarations
 *)
let printf_t = L.var_arg_function_type i32_t [| str_t |]

let printf_func = L.declare_function "printf" printf_t the_module

let codegen_print llbuilder el =
    L.build_call printf_func (Array.of_list el) "printf" llbuilder

let print_bool_t = L.function_type i32_t [| i1_t |]

let print_bool_f = L.declare_function "printBool" print_bool_t the_module

let print_bool e llbuilder =
    L.build_call print_bool_f [| e |] "print_bool" llbuilder

let codegen_string_lit s llbuilder =
    L.build_global_stringptr s "str_tmp" llbuilder

let create_node_t = L.var_arg_function_type node_t [| i32_t; i32_t |]

let create_node_f = L.declare_function "createNode" create_node_t the_module

let create_node (id, typ, nval) llbuilder =
    let actuals = [| id; lconst_of_typ typ; nval |]
    in L.build_call create_node_f actuals "node" llbuilder

let node_get_value_t = L.function_type void_ptr_t [| node_t; i32_t |]

let node_get_value_f =
    L.declare_function "nodeGetValue" node_get_value_t the_module

let node_get_value node typ llbuilder =
    let actuals = [| node; lconst_of_typ typ |] in
    let ret = L.build_call node_get_value_f actuals "nodeValue" llbuilder
    in
        match typ with
            | S.IntType -> void_to_int ret llbuilder
            | S.NumType -> void_to_float ret llbuilder
            | S.BoolType -> void_to_bool ret llbuilder
            | S.StringType -> void_to_string ret llbuilder
            | _ -> raise (Failure "[Error] Unsupported node value type.")

let print_node_t = L.function_type i32_t [| node_t |]

let print_node_f = L.declare_function "printNode" print_node_t the_module

let print_node node llbuilder =
    L.build_call print_node_f [| node |] "printNode" llbuilder

let context_funcs_vars = Hashtbl.create 50

let print_hashtbl tb =
    print_endline (Hashtbl.fold (fun k _ m -> k ^ (", " ^ m)) tb "")

(*
 * "Main" function of codegen, translates the program into it's LLVM IR equivalent
 *)
let translate program =
    let function_decls =
        let function_decl m fdecl =
            let name = fdecl.S.name
            and formal_types =
                Array.of_list (List.map (function | S.Formal (t, _) -> ltype_of_typ t) fdecl.S.args) in
            let ftype =
                L.var_arg_function_type (ltype_of_typ fdecl.S.returnType) formal_types
            in
            StringMap.add name ((L.define_function name ftype the_module), fdecl) m
            (*StringMap.add (fdecl.S.parent ^ "." ^ name) ((L.define_function name ftype the_module), fdecl) m*)
        in List.fold_left function_decl StringMap.empty program in
    
    (* Fill in the body of the given function *)
    let build_function_body fdecl =
        let get_var_name fname n = fname ^ ("." ^ n) in
        let (the_function, _) = StringMap.find fdecl.S.name function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let _ =
            let add_to_context locals =
                (ignore (Hashtbl.add context_funcs_vars fdecl.S.name locals);
                 (* ignore(print_hashtbl context_funcs_vars); *)
                 locals) in
            let add_formal m =
                function
                    | S.Formal (t, n) ->
                        (fun p ->
                             let n' = get_var_name fdecl.S.name n in
                             let local =
                                 L.define_global n' (get_default_value_of_type t) the_module
                             in
                                 (if L.is_null p
                                  then ()
                                  else ignore (L.build_store p local builder);
                                  StringMap.add n' (local, t) m)) in
            let add_local m =
                function
                    | S.Formal (t, n) ->
                        let n' = get_var_name fdecl.S.name n in
                        let local_var =
                            L.define_global n' (get_default_value_of_type t) the_module
                        in StringMap.add n' (local_var, t) m in
            let formals =
                List.fold_left2 add_formal StringMap.empty fdecl.S.args
                    (Array.to_list (L.params the_function))
            in add_to_context (List.fold_left add_local formals fdecl.S.locals) in

        (* Return the value for a variable or formal argument *)
        (* let lookup n = StringMap.find n local_vars in *)
        let lookup n =
            let get_parent_func_name fname =
                let (_, fdecl) = StringMap.find fname function_decls
                in fdecl.S.parent in
            let rec aux n fname =
                try
                    StringMap.find (get_var_name fname n)
                        (Hashtbl.find context_funcs_vars fname)
                with
                    | Not_found ->
                        if fname = "main"
                        then raise (Failure "[Error] Local Variable not found.")
                        else aux n (get_parent_func_name fname)
            in aux n fdecl.S.name in

        (* Construct code for an expression; return its value *)
        let handle_binop e1 op e2 dtype llbuilder =
            (* Generate llvalues from e1 and e2 *)
            let float_ops op e1 e2 =
                match op with
                    | S.Add -> L.build_fadd e1 e2 "flt_addtmp" llbuilder
                    | S.Sub -> L.build_fsub e1 e2 "flt_subtmp" llbuilder
                    | S.Mult -> L.build_fmul e1 e2 "flt_multmp" llbuilder
                    | S.Div -> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
                    | S.Mod -> L.build_frem e1 e2 "flt_sremtmp" llbuilder
                    | S.Equal -> L.build_fcmp L.Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
                    | S.Neq -> L.build_fcmp L.Fcmp.One e1 e2 "flt_neqtmp" llbuilder
                    | S.Leq -> L.build_fcmp L.Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
                    | S.LThan -> L.build_fcmp L.Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
                    | S.GThan -> L.build_fcmp L.Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
                    | S.Geq -> L.build_fcmp L.Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
                    | _ -> raise (Failure "[Error] Unrecognized float binop opreation.") in
            (* chars are considered ints, so they will use int_ops as well*)
            let int_ops op e1 e2 =
                match op with
                    | S.Add -> L.build_add e1 e2 "addtmp" llbuilder
                    | S.Sub -> L.build_sub e1 e2 "subtmp" llbuilder
                    | S.Mult -> L.build_mul e1 e2 "multmp" llbuilder
                    | S.Div -> L.build_sdiv e1 e2 "divtmp" llbuilder
                    | S.Mod -> L.build_srem e1 e2 "sremtmp" llbuilder
                    | S.Equal -> L.build_icmp L.Icmp.Eq e1 e2 "eqtmp" llbuilder
                    | S.Neq -> L.build_icmp L.Icmp.Ne e1 e2 "neqtmp" llbuilder
                    | S.Leq -> L.build_icmp L.Icmp.Sle e1 e2 "leqtmp" llbuilder
                    | S.LThan -> L.build_icmp L.Icmp.Slt e1 e2 "lesstmp" llbuilder
                    | S.GThan -> L.build_icmp L.Icmp.Sgt e1 e2 "sgttmp" llbuilder
                    | S.Geq -> L.build_icmp L.Icmp.Sge e1 e2 "sgetmp" llbuilder
                    | S.And -> L.build_and e1 e2 "andtmp" llbuilder
                    | S.Or -> L.build_or e1 e2 "ortmp" llbuilder
                    | _ -> raise (Failure "[Error] Unrecognized int binop opreation.") in
            let type_handler d =
                match d with
                    | S.NumType -> float_ops op e1 e2
                    | S.BoolType | S.IntType -> int_ops op e1 e2
                    | _ -> raise (Failure "[Error] Unrecognized binop data type.")
            in
                ((type_handler dtype),
                 (match op with
                     | S.Add | S.Sub | S.Mult | S.Div | S.Mod -> dtype
                     | _ -> S.BoolType)) in
        let rec expr builder =
            function
                | S.IntLit i -> ((L.const_int i32_t i), S.IntType)
                | S.NumLit f -> ((L.const_float f_t f), S.NumType)
                | S.BoolLit b -> ((L.const_int i1_t (if b then 1 else 0)), S.BoolType)
                | S.StringLit s -> ((codegen_string_lit s builder), S.StringType)
                | S.Noexpr -> ((L.const_int i32_t 0), S.VoidType)
                | S.Null -> (const_null, S.NullType)
                | S.Id s ->
                    let (var, typ) = lookup s in ((L.build_load var s builder), typ)
                | S.Node (id, e) ->
                    let (nval, typ) = expr builder e
                    in
                        ((create_node ((L.const_int i32_t id), typ, nval) builder), S.
                             NodeType)
                | S.Binop (e1, op, e2) ->
                    let (e1', t1) = expr builder e1
                    and (e2', t2) = expr builder e2
                    in
                        (* Handle Automatic Binop Type Converstion *)
                        (match (t1, t2) with
                            | (_, S.NullType) ->
                                (match op with
                                    | S.Equal ->
                                        ((L.build_is_null e1' "isNull" builder), S.BoolType)
                                    | S.Neq ->
                                        ((L.build_is_not_null e1' "isNull" builder), S.
                                             BoolType)
                                    | _ ->
                                        raise
                                            (Failure "[Error] Unsupported Null Type Operation."))
                            | (S.NullType, _) ->
                                (match op with
                                    | S.Equal ->
                                        ((L.build_is_null e2' "isNotNull" builder), S.BoolType)
                                    | S.Neq ->
                                        ((L.build_is_not_null e2' "isNotNull" builder), S.
                                             BoolType)
                                    | _ ->
                                        raise
                                            (Failure "[Error] Unsupported Null Type Operation."))
                            | (t1, t2) when t1 = t2 -> handle_binop e1' op e2' t1 builder
                            | (S.IntType, S.NumType) ->
                                handle_binop (int_to_float builder e1') op e2' S.NumType
                                    builder
                            | (S.NumType, S.IntType) ->
                                handle_binop e1' op (int_to_float builder e2') S.NumType
                                    builder
                            | _ -> raise (Failure "[Error] Unsuported Binop Type."))
                | S.Unop (op, e) ->
                    let (e', typ) = expr builder e
                    in
                        (((match op with
                             | S.Neg ->
                                 if typ = S.IntType then L.build_neg else L.build_fneg
                             | S.Not -> L.build_not) e' "tmp" builder),
                         typ)
                | S.Assign (s, e) ->
                    let (e', etyp) = expr builder e in
                    let (var, typ) = lookup s
                    in
                        ((match (etyp, typ) with
                             | (t1, t2) when t1 = t2 ->
                                 (ignore (L.build_store e' var builder); e')
                             | (S.NullType, _) ->
                                 (ignore
                                      (L.build_store (get_null_value_of_type typ) var builder);
                                  get_null_value_of_type typ)
                             | (S.IntType, S.NumType) ->
                                 let e' = int_to_float builder e'
                                 in (ignore (L.build_store e' var builder); e')
                             | _ -> raise (Failure "[Error] Assign Type inconsist.")),
                         typ)
                (* | S.CallObject (o, f, e) -> *)
                | S.Call ("print", el) ->
                    let print_expr e =
                        let (eval, etyp) = expr builder e
                        in
                            (match etyp with
                                | S.IntType ->
                                    ignore
                                        (codegen_print builder
                                             [ codegen_string_lit "%d\n" builder; eval ])
                                | S.NullType ->
                                    ignore
                                        (codegen_print builder
                                             [ codegen_string_lit "null\n" builder ])
                                | S.BoolType -> ignore (print_bool eval builder)
                                | S.NumType ->
                                    ignore
                                        (codegen_print builder
                                             [ codegen_string_lit "%f\n" builder; eval ])
                                | S.StringType ->
                                    ignore
                                        (codegen_print builder
                                             [ codegen_string_lit "%s\n" builder; eval ])
                                | S.NodeType -> ignore (print_node eval builder)
                                | _ -> raise (Failure "[Error] Unsupported type for print."))
                    in (List.iter print_expr el; ((L.const_int i32_t 0), S.VoidType))
                | S.Call ("printf", el) ->
                    ((codegen_print builder
                            (List.map (fun e -> let (eval, _) = expr builder e in eval) el)),
                     S.VoidType)
                | S.Call (f, act) ->
                    let (fdef, fdecl) = StringMap.find f function_decls in
                    let actuals =
                        List.rev
                            (List.map (fun e -> let (eval, _) = expr builder e in eval)
                                 (List.rev act)) in
                    let result =
                        (match fdecl.S.returnType with
                            | S.VoidType -> ""
                            | _ -> f ^ "_result")
                    in
                        ((L.build_call fdef (Array.of_list actuals) result builder),
                         (fdecl.S.returnType)) in
        (* Invoke "f builder" if the current block doesn't already
             have a terminal (e.g., a branch). *)
        let add_terminal builder f =
            match L.block_terminator (L.insertion_block builder) with
                | Some _ -> ()
                | None -> ignore (f builder) in
        (* Build the code for the given statement; return the builder for
             the statement's successor *)
        let rec stmt builder =
            function
                | S.Expr e -> (ignore (expr builder e); builder)
                | S.Return e ->
                    (ignore
                         (let (ev, et) = expr builder e
                          in
                              match ((fdecl.S.returnType), et) with
                                  | (S.VoidType, _) -> L.build_ret_void builder
                                  | (t1, t2) when t1 = t2 -> L.build_ret ev builder
                                  | (S.NumType, S.IntType) ->
                                      L.build_ret (int_to_float builder ev) builder
                                  | (t1, S.NullType) ->
                                      L.build_ret (get_default_value_of_type t1) builder
                                  | _ -> raise (Failure "[Error] Return type doesn't match."));
                     builder)
                | S.If (predicate, then_stmt, else_stmt) ->
                    let (bool_val, _) = expr builder predicate in
                    let merge_bb = L.append_block context "merge" the_function in
                    let then_bb = L.append_block context "then" the_function
                    in
                        (add_terminal
                             (List.fold_left stmt (L.builder_at_end context then_bb)
                                  then_stmt)
                             (L.build_br merge_bb);
                         let else_bb = L.append_block context "else" the_function
                         in
                             (add_terminal
                                  (List.fold_left stmt (L.builder_at_end context else_bb)
                                       else_stmt)
                                  (L.build_br merge_bb);
                              ignore (L.build_cond_br bool_val then_bb else_bb builder);
                              L.builder_at_end context merge_bb))
                | S.While (predicate, body) ->
                    let pred_bb = L.append_block context "while" the_function
                    in
                        (ignore (L.build_br pred_bb builder);
                         let body_bb = L.append_block context "while_body" the_function
                         in
                             (add_terminal
                                  (List.fold_left stmt (L.builder_at_end context body_bb)
                                       body)
                                  (L.build_br pred_bb);
                              let pred_builder = L.builder_at_end context pred_bb in
                              let (bool_val, _) = expr pred_builder predicate in
                              let merge_bb = L.append_block context "merge" the_function
                              in
                                  (ignore
                                       (L.build_cond_br bool_val body_bb merge_bb pred_builder);
                                   L.builder_at_end context merge_bb)))
                | S.For (e1, e2, e3, body) ->
                    List.fold_left stmt builder
                        [ S.Expr e1; S.While (e2, (body @ [ S.Expr e3 ])) ] in
        (* Build the code for each statement in the function *)
        let builder = List.fold_left stmt builder fdecl.S.body
        in
            (* Add a return if the last block falls off the end *)
            add_terminal builder
                (match fdecl.S.returnType with
                    | S.VoidType -> L.build_ret_void
                    | (S.IntType as t) -> L.build_ret (L.const_int (ltype_of_typ t) 0)
                    | (S.BoolType as t) -> L.build_ret (L.const_int (ltype_of_typ t) 0)
                    | (S.NumType as t) ->
                        L.build_ret (L.const_float (ltype_of_typ t) 0.)
                    | t -> L.build_ret (L.const_null (ltype_of_typ t)))
    in (List.iter build_function_body (List.rev program); the_module)
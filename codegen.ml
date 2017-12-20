module L = Llvm
module S = Sast

module StringMap = Map.Make(String)

let context     = L.global_context ()
let llctx       = L.global_context ()

let utilsBuffer = L.MemoryBuffer.of_file "utils.bc"
let llm         = Llvm_bitreader.parse_bitcode llctx utilsBuffer
let the_module  = L.create_module   context "PixMix"

let i32_t       = L.i32_type        context
and f_t         = L.double_type     context
and i8_t        = L.i8_type         context
and i1_t        = L.i1_type         context
and str_t       = L.pointer_type    (L.i8_type context)
and obj_t       = L.pointer_type    (L.i8_type context)
and void_t      = L.void_type       context
and void_ptr_t  = L.pointer_type    (L.i8_type context)

(*
let image_t = L.pointer_type (match L.type_by_name llm "struct.Image" with
    None -> raise (Failure "Image could not be found in the C libraries.")
  | Some x -> x)

let pixel_t = L.pointer_type (match L.type_by_name llm "struct.Pixel" with
    None -> raise (Failure "Pixel could not be found in the C libraries.")
  | Some x -> x)

let color_t = L.pointer_type (match L.type_by_name llm "struct.Color" with
    None -> raise (Failure "Color could not be found in the C libraries.")
  | Some x -> x)
*)

let rec ltype_of_typ = function
    | S.VoidType -> void_t
    | S.IntType -> i32_t
    | S.NumType -> f_t
    | S.BoolType -> i1_t
    | S.StringType -> str_t
    | S.ArrayType(typ) -> L.pointer_type (ltype_of_typ typ)
    | S.ObjectType -> obj_t
    | _ as t -> raise (Failure ("[Error] Could not find type \"" ^ S.string_of_varType t ^ "\"."))

let lconst_of_typ = function
    | S.IntType -> L.const_int i32_t 0
    | S.NumType -> L.const_int i32_t 1
    | S.BoolType -> L.const_int i32_t 2
    | S.StringType -> L.const_int i32_t 3
    | S.ImageType -> L.const_int i32_t 4
    | S.ArrayType(typ) -> L.const_int i32_t 5
    | S.ObjectType -> L.const_int i32_t 6
    | _ -> raise (Failure "[Error] Type Not Found for lconst_of_typ.")

let int_zero = L.const_int i32_t 0
and num_zero = L.const_float f_t 0.
and bool_false = L.const_int i1_t 0
and bool_true = L.const_int i1_t 1
and const_null = L.const_int i32_t 0
and str_null = L.const_null str_t
and object_null = L.const_null obj_t

let getNullForType = function
    | S.StringType -> str_null
    | S.NumType -> num_zero
    | S.ObjectType -> object_null
    | _ as x -> raise (Failure ("[Error] Could not get null type for " ^ S.string_of_varType x ^ "."))

let getDefaultValue = function
    | (S.IntType as t) -> L.const_int (ltype_of_typ t) 0
    | (S.BoolType as t) -> L.const_int (ltype_of_typ t) 0
    | (S.NumType as t) -> L.const_float (ltype_of_typ t) 0.
    | t -> L.const_null (ltype_of_typ t)

(*
    Type casting
 *)
let int_to_float llbuilder v = L.build_sitofp v f_t "tmp" llbuilder
let float_to_int llbuilder v = L.build_fptosi v i32_t "tmp" llbuilder

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

(*
    Built-in function declarations
 *)
let printf_t = L.var_arg_function_type i32_t [| str_t |]
let printf_func = L.declare_function "printf" printf_t the_module
let codegen_print llbuilder el = L.build_call printf_func (Array.of_list el) "printf" llbuilder

let print_bool_t = L.function_type i32_t [| i1_t |]
let print_bool_f = L.declare_function "printBool" print_bool_t the_module
let print_bool e llbuilder = L.build_call print_bool_f [| e |] "print_bool" llbuilder

let codegen_string_lit s llbuilder = L.build_global_stringptr s "str_tmp" llbuilder

let context_funcs_vars = Hashtbl.create 50

let print_hashtbl tb = print_endline (Hashtbl.fold (fun k _ m -> k ^ (", " ^ m)) tb "")

(*
    "Main" function of codegen, translates the program into it's LLVM IR equivalent
 *)
let translate program =
    let functionDecls =
        let funDecl m fdecl =
            let name = fdecl.S.name
            and formal_types =
                Array.of_list (List.map (function | S.Formal (t, _) -> ltype_of_typ t) fdecl.S.args) in
            let ftype =
                L.var_arg_function_type (ltype_of_typ fdecl.S.returnType) formal_types
            in
            StringMap.add name ((L.define_function name ftype the_module), fdecl) m
        in 
        List.fold_left funDecl StringMap.empty program 
    in
    
    let buildFunctionBody fdecl =
        let getVariableName fName vName = fName^"."^vName in
        let (func, _) = StringMap.find fdecl.S.name functionDecls in
        let builder = L.builder_at_end context (L.entry_block func) in

        let _ =
        let add_to_context locals = (ignore (Hashtbl.add context_funcs_vars fdecl.S.name locals); locals) in
        let addFormal m = function
            | S.Formal (t, vName) -> (fun p ->
                let vName' = getVariableName fdecl.S.name vName in
                let local = L.define_global vName' (getDefaultValue t) the_module in 
                (if L.is_null p
                    then ()
                    else ignore (L.build_store p local builder);
                    StringMap.add vName' (local, t) m
                )
            )
        in
        let addLocal m = function
            | S.Formal (t, vName) ->
                let vName' = getVariableName fdecl.S.name vName in
                let local_var = L.define_global vName' (getDefaultValue t) the_module in
                    StringMap.add vName' (local_var, t) m in
                let formals = List.fold_left2 addFormal StringMap.empty fdecl.S.args
                    (Array.to_list (L.params func))
        in 
        add_to_context (List.fold_left addLocal formals fdecl.S.locals)
    in

    let lookup vName =
        let getFunctionParentName fName =
            let (_, fdecl) = StringMap.find fName functionDecls
            in fdecl.S.parent
        in
        let rec findParent vName fName =
            try
                StringMap.find (getVariableName fName vName) (Hashtbl.find context_funcs_vars fName)
            with
                | Not_found ->
                    if fName = "main"
                    then raise (Failure ("[Error] Local variable " ^ (getVariableName fName vName) ^ " not found. ("^fName^")"))
                    else findParent vName (getFunctionParentName fName)
        in findParent vName fdecl.S.name 
    in

    (*
        Create the code for a binary operation
        e1 : the left-hand expression
        op : the operation
        e2 : the right-hand expression
        t  : the type that the operation should return
     *)
    let binop e1 op e2 t llbuilder =
        (* Generate llvalues from e1 and e2 *)
        let floatBinops op e1 e2 = match op with
            | S.Add -> L.build_fadd e1 e2 "flt_addtmp" llbuilder
            | S.Sub -> L.build_fsub e1 e2 "flt_subtmp" llbuilder
            | S.Mult -> L.build_fmul e1 e2 "flt_multmp" llbuilder
            | S.Div -> L.build_fdiv e1 e2 "flt_divtmp" llbuilder
            | S.Mod -> L.build_frem e1 e2 "flt_fremtmp" llbuilder
            | S.Equal -> L.build_fcmp L.Fcmp.Oeq e1 e2 "flt_eqtmp" llbuilder
            | S.Neq -> L.build_fcmp L.Fcmp.One e1 e2 "flt_neqtmp" llbuilder
            | S.Leq -> L.build_fcmp L.Fcmp.Ole e1 e2 "flt_leqtmp" llbuilder
            | S.LThan -> L.build_fcmp L.Fcmp.Ult e1 e2 "flt_lesstmp" llbuilder
            | S.GThan -> L.build_fcmp L.Fcmp.Ogt e1 e2 "flt_sgttmp" llbuilder
            | S.Geq -> L.build_fcmp L.Fcmp.Oge e1 e2 "flt_sgetmp" llbuilder
            | _ -> raise (Failure "[Error] Unrecognized float binop opreation.") 
        in
        let intBinops op e1 e2 = match op with
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
            | _ -> raise (Failure "[Error] Unrecognized int binop opreation.") 
        in
        let types t = match t with
            | S.NumType -> floatBinops op e1 e2
            | S.BoolType | S.IntType -> intBinops op e1 e2
            | _ -> raise (Failure "[Error] Unrecognized binop data type.")
        in ((types t), (match op with
            | S.Add | S.Sub | S.Mult | S.Div | S.Mod -> t
            | _ -> S.BoolType)) 
    in
    let rec expr builder = function
        | S.IntLit i -> ((L.const_int i32_t i), S.IntType)
        | S.NumLit f -> ((L.const_float f_t f), S.NumType)
        | S.BoolLit b -> ((L.const_int i1_t (if b then 1 else 0)), S.BoolType)
        | S.StringLit s -> ((codegen_string_lit s builder), S.StringType)
        | S.Noexpr -> ((L.const_int i32_t 0), S.VoidType)
        | S.Null -> (const_null, S.NullType)
        | S.Id s -> let (var, typ) = lookup s in ((L.build_load var s builder), typ)
        | S.Binop (e1, op, e2) ->
            let (e1', t1) = expr builder e1
            and (e2', t2) = expr builder e2
            in (match (t1, t2) with
                | (_, S.NullType) -> (match op with
                    | S.Equal -> ((L.build_is_null e1' "isNull" builder), S.BoolType)
                    | S.Neq -> ((L.build_is_not_null e1' "isNull" builder), S.BoolType)
                    | _ -> raise (Failure "[Error] Unsupported Null Type Operation."))
                | (S.NullType, _) -> (match op with
                    | S.Equal -> ((L.build_is_null e2' "isNotNull" builder), S.BoolType)
                    | S.Neq -> ((L.build_is_not_null e2' "isNotNull" builder), S.BoolType)
                    | _ -> raise (Failure "[Error] Unsupported Null Type Operation."))
                | (t1, t2) when t1 = t2 -> binop e1' op e2' t1 builder
                | (S.IntType, S.NumType) ->
                    binop (int_to_float builder e1') op e2' S.NumType builder
                | (S.NumType, S.IntType) ->
                    binop e1' op (int_to_float builder e2') S.NumType builder
                | _ -> raise (Failure "[Error] Unsuported Binop Type."))
        | S.Unop (op, e) ->
            let (e', typ) = expr builder e
            in (((match op with
                | S.Neg ->
                    if typ = S.IntType then L.build_neg else L.build_fneg
                | S.Not -> L.build_not) e' "tmp" builder
            ), typ)
        | S.Assign (s, e) ->
            let (e', etyp) = expr builder e in
            let (var, typ) = lookup s
            in ((match (etyp, typ) with
                | (t1, t2) when t1 = t2 -> (ignore (L.build_store e' var builder); e')
                | (S.NullType, _) -> (ignore (L.build_store (getNullForType typ) var builder);
                    getNullForType typ)
                | (S.IntType, S.NumType) -> 
                    let e' = int_to_float builder e'
                    in (ignore (L.build_store e' var builder); e')
                | _ -> raise (Failure "[Error] Assign Type inconsist.")
            ), typ)
        | S.ArrayAccess(e, i) ->
            let (arr, _) = expr builder e in
            let (index, _) = expr builder i in
            let ind = L.build_add index (L.const_int i32_t 1) "array_index" builder in
            let _val = L.build_gep arr [| ind |] "array_access" builder in
                (L.build_load _val "array_access_val" builder, S.IntType)
        | S.ArrayCreate(typ, _, e) -> 
            let t = ltype_of_typ typ in
            let (e', _) = expr builder e in
            let size = float_to_int builder e' in

            let size_t = L.build_intcast (L.size_of t) i32_t "tmp" builder in
            let size = L.build_mul size_t size "tmp" builder in
            let size_real = L.build_add size (L.const_int i32_t 1) "tmp" builder in
            
            let arr = L.build_array_malloc t size_real "tmp" builder in
            let arr = L.build_pointercast arr (L.pointer_type t) "tmp" builder in
            let arr_len_ptr = L.build_pointercast arr (L.pointer_type i32_t) "6tmp" builder in
                ignore(L.build_store size_real arr_len_ptr builder);
            (arr, typ) 
        | S.Call ("print", el) -> let print_expr e = 
            let (eval, etyp) = expr builder e in (match etyp with
                | S.IntType -> ignore (codegen_print builder [ codegen_string_lit "%d\n" builder; eval ])
                | S.NullType -> ignore (codegen_print builder [ codegen_string_lit "null\n" builder ])
                | S.BoolType -> ignore (print_bool eval builder)
                | S.NumType -> ignore (codegen_print builder [ codegen_string_lit "%f\n" builder; eval ])
                | S.StringType -> ignore (codegen_print builder [ codegen_string_lit "%s\n" builder; eval ])
                | _ -> raise (Failure "[Error] Unsupported type for print."))
            in (List.iter print_expr el; ((L.const_int i32_t 0), S.VoidType))
        | S.Call ("printf", el) ->
            ((codegen_print builder (List.map (fun e -> let (eval, _) = expr builder e in eval) el)), S.VoidType)
        | S.Call (f, act) ->
            let (fdef, fdecl) = StringMap.find f functionDecls in
            let actuals = List.rev (List.map (fun e -> let (eval, _) = expr builder e in eval) (List.rev act)) in
            let result = (match fdecl.S.returnType with
                | S.VoidType -> ""
                | _ -> f ^ "_result")
            in
            ((L.build_call fdef (Array.of_list actuals) result builder), (fdecl.S.returnType))
        | S.CallObject (o, f, act) ->
            let (fdef, fdecl) = StringMap.find f functionDecls in
            let actuals = List.rev (List.map (fun e -> let (eval, _) = expr builder e in eval) (List.rev act)) in
            let result = (match fdecl.S.returnType with
                | S.VoidType -> ""
                | _ -> f ^ "_result")
            in
            ((L.build_call fdef (Array.of_list actuals) result builder), (fdecl.S.returnType)) 
        | S.ObjectAccess(o, s) ->
            (*let (var, typ) = lookup (o^"."^s) in*)
            (const_null, S.NullType)
            (*((L.build_load var s builder), typ)*)
        in

    let add_terminal builder f = match L.block_terminator (L.insertion_block builder) with
        | Some _ -> ()
        | None -> ignore (f builder) in

    let rec stmt builder = function
        | S.Expr e -> (ignore (expr builder e); builder)
        | S.Return e -> (ignore (let (ev, et) = expr builder e in
            match ((fdecl.S.returnType), et) with
                | (S.VoidType, _) -> L.build_ret_void builder
                | (t1, t2) when t1 = t2 -> L.build_ret ev builder
                | (S.NumType, S.IntType) ->
                    L.build_ret (int_to_float builder ev) builder
                | (t1, S.NullType) ->
                    L.build_ret (getDefaultValue t1) builder
                | _ -> raise (Failure "[Error] Return type doesn't match."));
            builder)
        | S.If (predicate, then_stmt, else_stmt) ->
            let (bool_val, _) = expr builder predicate in
            let merge_bb = L.append_block context "merge" func in
            let then_bb = L.append_block context "then" func in 
                (add_terminal
                    (List.fold_left stmt (L.builder_at_end context then_bb) then_stmt)
                    (L.build_br merge_bb);
                    let else_bb = L.append_block context "else" func in
                        (add_terminal 
                            (List.fold_left stmt (L.builder_at_end context else_bb) else_stmt)
                            (L.build_br merge_bb);
                            ignore (L.build_cond_br bool_val then_bb else_bb builder); L.builder_at_end context merge_bb))
        | S.While (predicate, body) ->
            let pred_bb = L.append_block context "while" func in
                (ignore (L.build_br pred_bb builder);
                    let body_bb = L.append_block context "while_body" func in
                        (add_terminal (List.fold_left stmt (L.builder_at_end context body_bb) body) (L.build_br pred_bb);
                            let pred_builder = L.builder_at_end context pred_bb in
                            let (bool_val, _) = expr pred_builder predicate in
                            let merge_bb = L.append_block context "merge" func in
                                (ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder); L.builder_at_end context merge_bb)))
        | S.For (e1, e2, e3, body) -> List.fold_left stmt builder
            [ S.Expr e1; S.While (e2, (body @ [ S.Expr e3 ])) ] 
        | S.Object o -> builder
    in
    (* Build the code for each statement in the function *)
    let builder = List.fold_left stmt builder fdecl.S.body in
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.S.returnType with
        | S.VoidType -> L.build_ret_void
        | (S.IntType as t) -> L.build_ret (L.const_int (ltype_of_typ t) 0)
        | (S.BoolType as t) -> L.build_ret (L.const_int (ltype_of_typ t) 0)
        | (S.NumType as t) -> L.build_ret (L.const_float (ltype_of_typ t) 0.)
        | t -> L.build_ret (L.const_null (ltype_of_typ t)))
    in 
List.iter buildFunctionBody (List.rev program); 
the_module

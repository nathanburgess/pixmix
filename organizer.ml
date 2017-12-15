module A = Ast
module S = Sast
    
module StringMap = Map.Make(String)
    
let convertBinOp = function
    | A.Add     -> S.Add
    | A.Sub     -> S.Sub
    | A.Mult    -> S.Mult
    | A.Div     -> S.Div
    | A.Mod     -> S.Mod
    | A.Equal   -> S.Equal
    | A.Neq     -> S.Neq
    | A.Less    -> S.Less
    | A.Leq     -> S.Leq
    | A.Greater -> S.Greater
    | A.Geq     -> S.Geq
    | A.And     -> S.And
    | A.Or      -> S.Or
    
let convertUnOp = function | A.Neg -> S.Neg | A.Not -> S.Not
    
let convertVarType = function
    | A.NullType    -> S.NullType
    | A.VoidType    -> S.VoidType
    | A.IntType     -> S.IntType
    | A.FloatType   -> S.FloatType
    | A.StringType  -> S.StringType
    | A.BoolType    -> S.BoolType
    | A.NodeType    -> S.NodeType
    
let rec getName map aux curName =
    if StringMap.mem curName map
    then (let aux = (StringMap.find curName map) ^ ("." ^ aux) in 
        getName map aux (StringMap.find curName map))
    else aux
    
let rec convertExpr map = function
    | A.Null                    -> S.Null
    | A.Noexpr                  -> S.Noexpr
    | A.IntLit a                -> S.IntLit a
    | A.FloatLit a              -> S.FloatLit a
    | A.StringLit a             -> S.StringLit a
    | A.BoolLit a               -> S.BoolLit a
    | A.Binop (a, b, c)         -> S.Binop ((convertExpr map a), (convertBinOp b), (convertExpr map c))
    | A.Unop (a, b)             -> S.Unop ((convertUnOp a), (convertExpr map b))
    | A.Id a                    -> S.Id a
    | A.Assign (a, b)           -> S.Assign (a, (convertExpr map b))
    | A.Call (a, b)             -> S.Call ((getName map a a), (convertExprs map b))
    | A.CallDefault (a, b, c)   -> S.CallDefault ((convertExpr map a), b, (convertExprs map c))

and convertExprs map = function
    | [] -> []
    | [ x ] -> [ convertExpr map x ]
    | (_ as l) -> List.map (convertExpr map) l
    
let convertFormal = function 
    | A.Formal (v, s) -> S.Formal ((convertVarType v), s)
    
let buildFormals = function
    | [] -> []
    | [ x ] -> [ convertFormal x ]
    | (_ as l) -> List.map convertFormal l
    
let createMain stmts = A.Function 
    { 
        A.returnType = A.IntType; 
        A.name = "main"; 
        A.args = [];
        A.body = stmts;
    }
    
let rec getFunctionsA = function
    | [] -> []
    | ((A.Function _ as x)) :: tl -> x :: (getFunctionsA tl)
    | _ :: tl -> getFunctionsA tl
    
let rec getFunctionBodyA = function
    | [] -> []
    | A.Function _ :: tl -> getFunctionBodyA tl
    | ((_ as x)) :: tl -> x :: (getFunctionBodyA tl)
    
let rec mapper parent map = function
    | [] -> map
    | A.Function { A.name = n; _ } :: tl -> mapper parent (StringMap.add n parent map) tl
    | _ -> map
    
let buildFunctionBody map = function
    | A.Function { A.name = n; A.body = b; _ } ->
        let curr = getFunctionsA b in
        let map = mapper n map curr in (curr, map)
    | _ -> ([], map)
    
let rec buildFunction map result = function
    | [] -> ((List.rev result), map)
    | ((A.Function { A.returnType = r; A.name = n; A.args = args; A.body = b } as a)) :: tl ->
        let result1 = buildFunctionBody map a in
        let latterlist = tl @ (fst result1) in
        let map = snd result1 in
        let addedFunc = A.Function 
            {
                A.returnType = r;
                A.name = n;
                A.args = args;
                A.body = getFunctionBodyA b;
            } 
        in
        let result = result @ [ addedFunc ] in buildFunction map result latterlist
    | _ -> ([], map)
    
let rec convertStatement map = function
    | A.Expr a -> S.Expr (convertExpr map a)
    | A.Return a -> S.Return (convertExpr map a)
    | A.For (e1, e2, e3, stls) ->
        S.For ((convertExpr map e1), (convertExpr map e2), (convertExpr map e3), (List.map (convertStatement map) stls))
    | A.If (e, stls1, stls2) ->
        S.If ((convertExpr map e), (List.map (convertStatement map) stls1), (List.map (convertStatement map) stls2))
    | A.While (e, stls) -> S.While ((convertExpr map e), (List.map (convertStatement map) stls))
    | _ -> S.Expr S.Noexpr
    
let rec getFunctionBodyS map = function
    | [] -> []
    | A.Variable (A.Local (_, name, v)) :: tl when v <> A.Noexpr ->
        (S.Expr (S.Assign (name, (convertExpr map v)))) :: (getFunctionBodyS map tl)
    | A.Variable (A.Local (_, _, v)) :: tl when v = A.Noexpr -> getFunctionBodyS map tl
    | ((_ as x)) :: tl -> (convertStatement map x) :: (getFunctionBodyS map tl)
    
let rec getFunctionLocals = function
    | [] -> []
    | A.Variable (A.Local (typ, name, _)) :: tl ->
        (S.Formal ((convertVarType typ), name)) :: (getFunctionLocals tl)
    | _ :: tl -> getFunctionLocals tl
    
let rec convertFunctionList map = function
    | [] -> []
    | A.Function { A.returnType = r; A.name = n; A.args = a; A.body = b } :: tl -> 
        {
            S.returnType = convertVarType r;
            S.name = getName map n n;
            S.args = buildFormals a;
            S.body = getFunctionBodyS map b;
            S.locals = getFunctionLocals b;
            S.parent = if n = "main" then "main"
                else getName map (StringMap.find n map) (StringMap.find n map);
        } :: (convertFunctionList map tl)
    | _ :: tl -> convertFunctionList map tl
    
(* entry point *)
let convert stmts =
    let funcs = createMain stmts in
        let funcList = buildFunction StringMap.empty [] [ funcs ] in
            convertFunctionList (snd funcList) (fst funcList)
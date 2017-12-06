module A = Ast

let convert ast = function
    | [] -> []

type sexpr =
      SLiteral      of int
    | SBoolLit      of bool
    | SStringLit    of string
    | SId           of string
    | SBinop        of expr * op * expr
    | SArrayCreate  of varType * expr list
    | SArrOp        of string * expr
    | SObjLit       of string * string
    | SObjCall      of string * string * expr list
    | SUnop         of uop * expr
    | SCall         of string * expr list
    | Null
    | This
    | Noexpr

type sstmt =
      SBlock        of sstmt list
    | SExpr         of sexpr
    | SReturn       of sexpr
    | SIf           of sexpr * sstmt * sstmt
    | SFor          of sexpr * sexpr * sexpr * sstmt
    | SWhile        of sexpr * sstmt

type sfunDecl = {
    sfnreturnType    : varType;
    sfnName          : string;
    sfnParameters    : bind list;
    sfnLocals        : bind list;
    sfnBody          : stmt list;
}

type sobjdecl =
    | (*List object declaration components to resolve *)

type sprogram =
    | (*List program components to resolve *)

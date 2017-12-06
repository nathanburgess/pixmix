module A = Ast

let convert (program:A.program) = 

    let wrapMain = A.Function({
        A.fnReturnType  = A.Int;
        A.fnName        = "main";
        A.fnParameters  = [];
        A.fnLocals      = program.variables;
        A.fnBody        = program.statements;
    }) in

    (*  *)
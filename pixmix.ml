(*
    Authors:
    Nathan Burgess
 *)

module StringMap = Map.Make(String)

type action = Ast | Sast | LLVM_IR | Compile

let _ =
    let action = ref Compile in
    let set_action a () = action := a in
    let speclist = [
        ("-a", Arg.Unit (set_action Ast), "Print the AST");
        ("-s", Arg.Unit (set_action Sast), "Print the AST");
        ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
        ("-c", Arg.Unit (set_action Compile),
         "Check and print the generated LLVM IR (default)");
    ] in  
    let usage_msg = "usage: ./pixmix.native [-a|-l|-c] [file.pm]" in
    let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Sast.convert ast in
        Semant.check sast;

    match !action with
        | Ast     -> print_string (Ast.stringOfProgram ast)
        | Sast    -> print_string (Sast.stringOfProgram sast)
        | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
        | Compile -> let m = Codegen.translate sast in
            Llvm_analysis.assert_valid_module m;
            print_string (Llvm.string_of_llmodule m)

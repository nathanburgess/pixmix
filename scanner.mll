(* Ocamllex scanner for PixMix *)

{ 
    open Parser 
    let unescape s =
        Scanf.sscanf("\"" ^ s ^ "\"") "%S%!" (fun x-> x)
}

let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']

rule token = parse
      [' ' '\t' '\r' '\n']  { token lexbuf }      (* Whitespace *)
    | "#:"                  { mlcomment lexbuf }  (* Comments *)
    | "#"                   { comment lexbuf }    (* Comments *)
    | '.'                   { DOT }
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | '['                   { LSQ_BRACE }
    | ']'                   { RSQ_BRACE }
    | '{'                   { LC_BRACE }
    | '}'                   { RC_BRACE }
    | ';'                   { SEMI }
    | ','                   { COMMA }
    | '+'                   { PLUS }
    | '-'                   { MINUS }
    | '*'                   { TIMES }
    | '/'                   { DIVIDE }
    | '='                   { ASSIGN }
    | "=="                  { EQ }
    | "!="                  { NEQ }
    | '<'                   { LT }
    | "<="                  { LEQ }
    | ">"                   { GT }
    | ">="                  { GEQ }
    | "&&"                  { AND }
    | "||"                  { OR }
    | "!"                   { NOT }
    | "if"                  { IF }
    | "else"                { ELSE }
    | "for"                 { FOR }
    | "while"               { WHILE }
    | "return"              { RETURN }
    | "num"                 { NUM }
    | "bool"                { BOOL }
    | "void"                { VOID }
    | "string"              { STRING }
    | "true"                { TRUE }
    | "false"               { FALSE }
    | "Object"              { OBJECT }
    | "this"                { THIS }
    | ['0'-'9']+ as lxm     { LITERAL(float_of_string lxm) }
    | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
    | '"'((ascii|escape)* as s)'"'{ STRLIT(unescape s) }
    | eof                   { EOF }
    | _ as char             { raise (Failure("illegal character " ^ Char.escaped char)) }
and mlcomment = parse
      ":#" { token lexbuf }
    | _    { mlcomment lexbuf }
and comment = parse
      ['\n' '\r'] { token lexbuf }
    | _    { comment lexbuf }
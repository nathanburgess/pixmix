(* Ocamllex scanner for PixMix *)

{ open Parser }

rule token = parse
      [' ' '\t' '\r' '\n']  { token lexbuf }      (* Whitespace *)
    | "#:"                  { mlcomment lexbuf }  (* Comments *)
    | "#"                   { comment lexbuf }    (* Comments *)
    | '.'                   { DOT }
    | '('                   { LPAREN }
    | ')'                   { RPAREN }
    | '['                   { LBRACKET }
    | ']'                   { RBRACKET }
    | '{'                   { LBRACE }
    | '}'                   { RBRACE }
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
    | ['0'-'9']+ as lxm     { LITERAL(float_of_string lxm) }
    | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
    | '"'((_*) as s)'"' { STRLIT(s) }
    | eof                   { EOF }
    | _ as char             { raise (Failure("illegal character " ^ Char.escaped char)) }
and mlcomment = parse
      ":#" { token lexbuf }
    | _    { mlcomment lexbuf }
and comment = parse
      ['\n' '\r'] { token lexbuf }
    | _    { comment lexbuf }
{
  open Parser
  let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

let digit       = ['0'-'9']
let letter      = ['a'-'z' 'A'-'Z']
let variable    = (letter | ('_' letter)) (letter | digit | '_') *
let escape      = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii       = ([' '-'!' '#'-'[' ']'-'~'])

rule token = parse 
    | [' ' '\t' '\r' '\n']                  { token lexbuf }
    | "#:"                                  { commentMl lexbuf }
    | "#"                                   { comment lexbuf }
    | "+"                                   { PLUS }
    | "-"                                   { MINUS }
    | "*"                                   { TIMES }
    | "/"                                   { DIVIDE }
    | "%"                                   { MOD }
    | ";"                                   { SEMI }
    | ","                                   { COMMA }
    | "="                                   { ASSIGN }
    | ":"                                   { COLON }
    | "."                                   { DOT }
    | "and"                                 { AND }
    | "&&"                                  { AND }
    | "or"                                  { OR }
    | "||"                                  { OR }
    | "not"                                 { NOT }
    | "!"                                   { NOT }
    | "if"                                  { IF }
    | "else"                                { ELSE }
    | "for"                                 { FOR }
    | "while"                               { WHILE}
    | "break"                               { BREAK }
    | "continue"                            { CONTINUE }
    | "in"                                  { IN }
    | "return"                              { RETURN }
    | ">"                                   { GT }
    | ">="                                  { GEQ }
    | "<"                                   { LT }
    | "<="                                  { LEQ }
    | "=="                                  { EQUAL}
    | "is"                                  { EQUAL}
    | "!="                                  { NEQ }
    | "isnt"                                { NEQ }
    | "void"                                { VOID }
    | "num"                                 { NUM }
    
    (*| "int"                                 { INT }
    | "float"                               { FLOAT }*)

    | "string"                              { STRING }
    | "bool"                                { BOOL }
    | "node"                                { NODE }
    | "null"                                { NULL }
    | "Array"                               { ARRAY }
    | "Object"                              { OBJECT }
    | "Image"                               { IMAGE }
    | "Pixel"                               { PIXEL }
    | "Color"                               { COLOR }
    | "Console"                             { CONSOLE }

    | digit+'.'?digit* as lit               { NUM_LITERAL(float_of_string lit) }
    | '"' ((ascii | escape)* as lit) '"'    { STRING_LITERAL(unescape lit) }
    | '"'                                   { QUOTE }
    | "true" | "false" as boolLit           { BOOL_LITERAL(bool_of_string boolLit)}

    | "["                                   { LSQUARE }
    | "]"                                   { RSQUARE }
    | "{"                                   { LCURL }
    | "}"                                   { RCURL }
    | "("                                   { LPAREN }
    | ")"                                   { RPAREN }
    | variable as id                        { ID(id) }
    | eof                                   { EOF }

and commentMl = parse 
    | ":#" {token lexbuf}
    | _ {commentMl lexbuf}

and comment = parse 
    | _ {comment lexbuf}

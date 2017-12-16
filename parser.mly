
%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE MOD SEMI COMMA ASSIGN COLON DOT
%token GT GEQ LT LEQ EQUAL NEQ AND OR NOT IF ELSE FOR WHILE BREAK 
%token CONTINUE IN RETURN QUOTE LSQUARE RSQUARE LCURL RCURL LPAREN 
%token RPAREN VOID NULL INT FLOAT STRING BOOL NODE ARRAY OBJECT IMAGE 
%token PIXEL COLOR CONSOLE EOF

/* Identifiers */
%token <string> ID

/* Literals */
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> BOOL_LITERAL

/* Order */
%right ASSIGN
%left AND OR
%left EQUAL NEQ
%left GT LT GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%right LINK RIGHTLINK LEFTLINK AMPERSAND
%left SIMILARITY AT
%right LPAREN
%left  RPAREN
%right COLON
%right DOT

%start program
%type <Ast.program> program

%%

program: stmntList EOF              { List.rev $1 }

stmntList:
| /* nothing */                      { [] }
| stmntList stmnt                    { $2 :: $1 }

stmnt:
| expr SEMI                         { Expr($1) }
| func_decl                         { Function($1) }
| RETURN SEMI                       { Return(Noexpr) }
| RETURN expr SEMI                  { Return($2) }
| FOR LPAREN forEx SEMI expr SEMI forEx RPAREN LCURL stmntList RCURL
  { For($3, $5, $7, List.rev $10) }
| IF LPAREN expr RPAREN LCURL stmntList RCURL ELSE LCURL stmntList RCURL
  { If($3,List.rev $6,List.rev $10) }
| IF LPAREN expr RPAREN LCURL stmntList RCURL
  { If($3,List.rev $6,[]) }
| WHILE LPAREN expr RPAREN LCURL stmntList RCURL
  {While($3, List.rev $6)}
| var_decl SEMI                     { Variable($1) }

var_decl:
| var_type ID                       { Local($1, $2, Noexpr) }
| var_type ID ASSIGN expr           { Local($1, $2, $4) }

var_type:
| NULL                              { NullType }
| VOID                              { VoidType }
| INT                               { IntType }
| FLOAT                             { FloatType }
| STRING                            { StringType }
| BOOL                              { BoolType }
| NODE                              { NodeType }
| arrayType                         { $1 }

arrayType : 
| LSQUARE var_type RSQUARE			{ ArrayType($2) }

formal_list:
| /* nothing */                     { [] }
| formal                            { [$1] }
| formal_list COMMA formal          { $3 :: $1 }

formal:
| var_type ID                       { Formal($1, $2) }

func_decl:
| var_type ID LPAREN formal_list RPAREN LCURL stmntList RCURL 
{ { returnType = $1;
    name = $2;
    args = List.rev $4;
    body = List.rev $7 } }

forEx:
| /* nothing */                     { Noexpr }
| expr                              { $1 }

expr:
| literals                          {$1}
| NULL                              { Null }
| arith_ops                         { $1 }
| NODE LPAREN expr RPAREN           { Node($3) }
| ID                                { Id($1) }
| ID ASSIGN expr                    { Assign($1, $3) }
| LPAREN expr RPAREN 	            { $2 }
| ID LPAREN list RPAREN             { Call($1, List.rev $3) }
| arrCreate                         { ArrayCreate(fst $1, snd $1) }
| expr arrAccess                    { ArrayAccess($1, $2) }

arrCreate:
| LSQUARE var_type COMMA expr RSQUARE { ( $2, [$4]) } 

arrAccess:
| LSQUARE expr RSQUARE { $2 }

list:
| /* nothing */                     { [] }
| expr                              { [$1] }
| list COMMA expr                   { $3 :: $1 }

arith_ops:
| expr PLUS         expr            { Binop($1, Add,   $3) }
| expr MINUS        expr            { Binop($1, Sub,   $3) }
| expr TIMES        expr            { Binop($1, Mult,  $3) }
| expr DIVIDE       expr            { Binop($1, Div,   $3) }
| expr EQUAL        expr            { Binop($1, Equal, $3) }
| expr NEQ          expr            { Binop($1, Neq,   $3) }
| expr LT           expr            { Binop($1, Less,  $3) }
| expr LEQ          expr            { Binop($1, Leq,   $3) }
| expr GT           expr            { Binop($1, Greater,  $3) }
| expr GEQ          expr            { Binop($1, Geq,   $3) }
| expr AND          expr            { Binop($1, And,   $3) }
| expr MOD          expr            { Binop($1, Mod,   $3) }
| expr OR           expr            { Binop($1, Or,    $3) }
| NOT               expr            { Unop (Not,   $2) }
| MINUS             expr            { Unop (Neg, $2) }

literals:
| INT_LITERAL                       { IntLit($1) }
| FLOAT_LITERAL                     { FloatLit($1) }
| STRING_LITERAL                    { StringLit($1) }
| BOOL_LITERAL                      { BoolLit($1) }

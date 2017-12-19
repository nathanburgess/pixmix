
%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE MOD SEMI COMMA ASSIGN COLON DOT
%token GT GEQ LT LEQ EQUAL NEQ AND OR NOT IF ELSE FOR WHILE BREAK 
%token CONTINUE IN RETURN LSQUARE RSQUARE LCURL RCURL LPAREN 
%token RPAREN VOID NULL INT NUM STRING BOOL ARRAY OBJECT IMAGE 
%token PIXEL COLOR CONSOLE EOF

/* Identifiers */
%token <string> ID

/* Literals */
%token <int> INT_LITERAL
%token <float> NUM_LITERAL
%token <string> STRING_LITERAL
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

program: stmtList EOF                           { List.rev $1 }

stmtList:
    | /* nothing */                             { [] }
    | stmtList stmt                             { $2 :: $1 }

stmt:
    | expr SEMI                                 { Expr($1) }
    | varDecl SEMI                              { Variable($1) }
    | OBJECT objDecl SEMI                       { Object($2) }
    | funcDecl                                  { Function($1) }
    | RETURN SEMI                               { Return(Noexpr) }
    | RETURN expr SEMI                          { Return($2) }
    | FOR LPAREN forExpr SEMI expr SEMI forExpr RPAREN LCURL stmtList RCURL
        { For($3, $5, $7, List.rev $10) }
    | IF LPAREN expr RPAREN LCURL stmtList RCURL ELSE LCURL stmtList RCURL
        { If($3,List.rev $6,List.rev $10) }
    | IF LPAREN expr RPAREN LCURL stmtList RCURL
        { If($3,List.rev $6,[]) }
    | WHILE LPAREN expr RPAREN LCURL stmtList RCURL
        { While($3, List.rev $6) }

objDecl:
    | ID stmtList                               { { 
        objName         = $1;
        objStmts        = [] } }
    | ID ASSIGN LCURL stmtList RCURL            { { 
        objName         = $1;
        objStmts        = List.rev $4 } }

varDecl:       
    | varType ID                                { Local($1, $2, Noexpr) }
    | varType ID ASSIGN expr                    { Local($1, $2, $4) }
        
varType:       
    | NULL                                      { NullType }
    | VOID                                      { VoidType }
    | INT                                       { IntType }
    | NUM                                       { NumType }
    | STRING                                    { StringType }
    | BOOL                                      { BoolType }
    | arrayType                                 { $1 }

arrayType:
    | LSQUARE varType RSQUARE                   { ArrayType($2) }

formalexprList:
    | /* nothing */                             { [] }
    | formal                                    { [$1] }
    | formalexprList COMMA formal               { $3 :: $1 }

formal:
    | varType ID                                { Formal($1, $2) }

funcDecl:
    | varType ID LPAREN formalexprList RPAREN LCURL stmtList RCURL { { 
        returnType = $1;
        name = $2;
        args = List.rev $4;
        body = List.rev $7 } }

forExpr:
    | /* nothing */                             { Noexpr }
    | expr                                      { $1 }

expr:       
    | literals                                  { $1 }
    | NULL                                      { Null }
    | expr PLUS         expr                    { Binop($1, Add,   $3) }
    | expr MINUS        expr                    { Binop($1, Sub,   $3) }
    | expr TIMES        expr                    { Binop($1, Mult,  $3) }
    | expr DIVIDE       expr                    { Binop($1, Div,   $3) }
    | expr EQUAL        expr                    { Binop($1, Equal, $3) }
    | expr NEQ          expr                    { Binop($1, Neq,   $3) }
    | expr LEQ          expr                    { Binop($1, Leq,   $3) }
    | expr LT           expr                    { Binop($1, LThan, $3) }
    | expr GT           expr                    { Binop($1, GThan, $3) }
    | expr GEQ          expr                    { Binop($1, Geq,   $3) }
    | expr AND          expr                    { Binop($1, And,   $3) }
    | expr MOD          expr                    { Binop($1, Mod,   $3) }
    | expr OR           expr                    { Binop($1, Or,    $3) }
    | NOT               expr                    { Unop(Not, $2) }
    | MINUS             expr                    { Unop(Neg, $2) }
    | ID                                        { Id($1) }
    | ID ASSIGN expr                            { Assign($1, $3) }
    | LPAREN expr RPAREN 	                    { $2 }
    | ID LPAREN exprList RPAREN                 { Call($1, List.rev $3) }
    | ID DOT ID LPAREN exprList RPAREN          { CallObject($1, $3, List.rev $5) }
    | arrCreate                                 { ArrayCreate($1) }
    | expr arrAccess                            { ArrayAccess($1, $2) }

arrCreate:
    | LSQUARE expr RSQUARE                      { $2 } /* Add other arrCreate with exprlist */


arrAccess:
    | LSQUARE expr RSQUARE                      { $2 }

exprList:
    | /* nothing */                             { [] }
    | expr                                      { [$1] }
    | exprList COMMA expr                       { $3 :: $1 }

literals:
	| NUM_LITERAL                              { NumLit($1) }
	| STRING_LITERAL                           { StringLit($1) }
	| BOOL_LITERAL                             { BoolLit($1) }
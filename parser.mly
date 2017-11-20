%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token STRING CHAR FLOAT IMAGE COLOR PIXEL OBJECT ARRAY
%token LBRACKET RBRACKET DOT
%token <int> LITERAL
%token <string> STRLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
    decls EOF                   { $1 }
        
decls:      
      /* nothing */             { { variables = []; objects = []; statements = []; functions = []; } }
    | decls vdecl               { { $1 with variables = $2::$1.variables } }
    | decls odecl               { { $1 with objects = $2::$1.objects } }
    | decls sdecl               { { $1 with statements = $2::$1.statements } }
    | decls fdecl               { { $1 with functions = $2::$1.functions } }

vdecl:
      typ ID SEMI               { ($1, $2) }
    /* | type ASSIGN expr SEMI { ()}  -- to assign at instantiation */

vdecl_list:
      /* nothing */             { [] }
    | vdecl_list vdecl          { $2 :: $1 }

odecl:
    OBJECT ID LBRACE vdecl_list stmt_list RBRACE
    { { oname    = $2;
        olocals  = List.rev $4;
        omethods = List.rev $5 }}

sdecl:
      expr SEMI                 { Expr $1 }
    | RETURN SEMI               { Return Noexpr }
    | RETURN expr SEMI          { Return $2 }
    | LBRACE stmt_list RBRACE   { Block(List.rev $2) }
    | IF LPAREN expr RPAREN sdecl %prec NOELSE 
        { If($3, $5, Block([])) }
    | IF LPAREN expr RPAREN sdecl ELSE sdecl    
        { If($3, $5, $7) }
    | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN sdecl
        { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN sdecl 
        { While($3, $5) }

stmt_list:
      /* nothing */             { [] }
    | stmt_list sdecl            { $2 :: $1 }

fdecl:
    typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { typ     = $1;
        fname   = $2;
        formals = $4;
        locals  = List.rev $7;
        body    = List.rev $8 } }

formals_opt:
      /* nothing */             { [] }
    | formal_list               { List.rev $1 }

formal_list:
      typ ID                    { [($1,$2)] }
    | formal_list COMMA typ ID  { ($3,$4) :: $1 }

typ:
      INT                       { Int }
    | BOOL                      { Bool }
    | VOID                      { Void }
    | STRING                    { String }
    | FLOAT                     { Float }
    | CHAR                      { Char }
    | OBJECT                    { Object }
    | ARRAY                     { Array }
    | IMAGE                     { Image }
    | PIXEL                     { Pixel }
    | COLOR                     { Color }

expr_opt:
      /* nothing */             { Noexpr }
    | expr                      { $1 }

expr:
      LITERAL                   { Literal($1) }
    | STRLIT                    { StringLit($1) }
    | TRUE                      { BoolLit(true) }
    | FALSE                     { BoolLit(false) }
    | ID                        { Id($1) }

    | ID LBRACKET expr RBRACKET { Arrop($1, $3) }
    | ID DOT ID                 { ObjLit($1, $3) }
    | ID DOT ID LPAREN actuals_opt RPAREN 
        { ObjCall($1, $3, $5) }    

    | expr PLUS   expr          { Binop($1, Add,   $3) }
    | expr MINUS  expr          { Binop($1, Sub,   $3) }
    | expr TIMES  expr          { Binop($1, Mult,  $3) }
    | expr DIVIDE expr          { Binop($1, Div,   $3) }
    | expr EQ     expr          { Binop($1, Equal, $3) }
    | expr NEQ    expr          { Binop($1, Neq,   $3) }
    | expr LT     expr          { Binop($1, Less,  $3) }
    | expr LEQ    expr          { Binop($1, Leq,   $3) }
    | expr GT     expr          { Binop($1, Greater, $3) }
    | expr GEQ    expr          { Binop($1, Geq,   $3) }
    | expr AND    expr          { Binop($1, And,   $3) }
    | expr OR     expr          { Binop($1, Or,    $3) }
    | MINUS expr %prec NEG      { Unop(Neg, $2) }
    | NOT expr                  { Unop(Not, $2) }
    | ID ASSIGN expr            { Assign($1, $3) }
    | ID LPAREN actuals_opt RPAREN 
        { Call($1, $3) }
    | LPAREN expr RPAREN        { $2 }

actuals_opt:
      /* nothing */             { [] }
    | actuals_list              { List.rev $1 }

actuals_list:
     expr                       { [$1] }
    | actuals_list COMMA expr   { $3 :: $1 }
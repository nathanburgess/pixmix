%{ open Ast %}

%token SEMI LPAREN RPAREN LC_BRACE RC_BRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE NUM BOOL VOID
%token STRING CHAR INT IMAGE COLOR PIXEL OBJECT THIS
%token ARRAY LSQ_BRACE RSQ_BRACE DOT
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
    decls EOF                               { $1 }
                    
decls:                  
      /* nothing */                         { { variables = []; objects = []; statements = []; functions = []; } }
    | decls varDecl                         { { $1 with variables   = $2::$1.variables  } }
    | decls objDecl                         { { $1 with objects     = $2::$1.objects    } }
    | decls stmtDecl                        { { $1 with statements  = $2::$1.statements } }
    | decls fnDecl                          { { $1 with functions   = $2::$1.functions  } }
            
varBind:            
    varType ID                              { ($1, $2) }
            
varDecl:            
      varBind SEMI                          { ($1) }
    /* @TODO - Get this working somehow
    | varBind ASSIGN expr SEMI  { ($1, $3) } */

varDeclList:
      /* nothing */                         { [] }
    | varDeclList varDecl                   { $2 :: $1 }

objDecl:
    OBJECT ID SEMI
    { { objName     = $2;
        objLocals   = [];
        objMethods     = [] } }

stmtDecl:
      expr SEMI                             { Expr $1 }
    | RETURN SEMI                           { Return Noexpr }
    | RETURN expr SEMI                      { Return $2 }
    | LC_BRACE statementsList RC_BRACE      { Block(List.rev $2) }
    | IF LPAREN expr RPAREN stmtDecl %prec NOELSE 
        { If($3, $5, Block([])) }
    | IF LPAREN expr RPAREN stmtDecl ELSE stmtDecl    
        { If($3, $5, $7) }
    | FOR LPAREN optionalExpr SEMI expr SEMI optionalExpr RPAREN stmtDecl
        { For($3, $5, $7, $9) }
    | WHILE LPAREN expr RPAREN stmtDecl     { While($3, $5) }

fnDecl:
    varType ID LPAREN optionalParameters RPAREN LC_BRACE varDeclList statementsList RC_BRACE
    { { fnReturnType    = $1;
        fnName          = $2;
        fnParameters    = $4;
        fnLocals        = List.rev $7;
        fnBody          = List.rev $8 } }

statementsList:
      /* nothing */                         { [] }
    | statementsList stmtDecl               { $2 :: $1 }
            
fnDeclList:         
    /* nothing */                           { [] }
    | fnDeclList fnDecl                     { $2 :: $1}
            
optionalParameters:         
      /* nothing */                         { [] }
    | parametersList                        { List.rev $1 }
            
parametersList:         
      varType ID                            { [($1, $2)] }
    | parametersList COMMA varType ID       { ($3, $4) :: $1 }

varType:
      NUM                                   { Num }
    | BOOL                                  { Bool }
    | CHAR                                  { Char }
    | STRING                                { String }
    | VOID                                  { Void }
    | COLOR                                 { Color }
    | IMAGE                                 { Image }
    | OBJECT                                { Object }
    | PIXEL                                 { Pixel }
    | ARRAY                                 { Array }
            
optionalExpr:           
      /* nothing */                         { Noexpr }
    | expr                                  { $1 }
            
expr:           
      LITERAL                               { Literal($1) }
    | STRLIT                                { StringLit($1) }
    | TRUE                                  { BoolLit(true) }
    | FALSE                                 { BoolLit(false) }
    | ID                                    { Id($1) }
    | ID LSQ_BRACE expr RSQ_BRACE           { ArrOp($1, $3) }
    | ID DOT ID                             { ObjLit($1, $3) }
    | ID DOT ID LPAREN actuals_opt RPAREN   { ObjCall($1, $3, $5) }    
    | expr PLUS   expr                      { Binop($1, Add,   $3) }
    | expr MINUS  expr                      { Binop($1, Sub,   $3) }
    | expr TIMES  expr                      { Binop($1, Mult,  $3) }
    | expr DIVIDE expr                      { Binop($1, Div,   $3) }
    | expr EQ     expr                      { Binop($1, Equal, $3) }
    | expr NEQ    expr                      { Binop($1, Neq,   $3) }
    | expr LT     expr                      { Binop($1, Less,  $3) }
    | expr LEQ    expr                      { Binop($1, Leq,   $3) }
    | expr GT     expr                      { Binop($1, Greater, $3) }
    | expr GEQ    expr                      { Binop($1, Geq,   $3) }
    | expr AND    expr                      { Binop($1, And,   $3) }
    | expr OR     expr                      { Binop($1, Or,    $3) }
    | MINUS expr %prec NEG                  { Unop(Neg, $2) }
    | NOT expr                              { Unop(Not, $2) }
    | ID ASSIGN expr                        { Assign($1, $3) }
    | ID LPAREN actuals_opt RPAREN          { Call($1, $3) }
    | LPAREN expr RPAREN                    { $2 }
            
actuals_opt:            
      /* nothing */                         { [] }
    | actuals_list                          { List.rev $1 }
            
actuals_list:           
     expr                                   { [$1] }
    | actuals_list COMMA expr               { $3 :: $1 }
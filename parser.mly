%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE NUM BOOL VOID
%token STRING CHAR FLOAT IMAGE COLOR PIXEL OBJECT ARRAY
%token LBRACKET RBRACKET DOT
<<<<<<< HEAD
<<<<<<< HEAD
%token <int> LITERAL
=======
=======
>>>>>>> c0fed97e34c376c4d8d244c6b167da1cc5c5d000
%token <float> LITERAL
%token <string> STRLIT
>>>>>>> fc75dadf11640f601a554a9d7f09835fd42a7964
%token <string> ID
%token <string> STRLIT
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
    | decls varDecl             { { $1 with variables   = $2::$1.variables  } }
    | decls objDecl             { { $1 with objects     = $2::$1.objects    } }
    | decls stmtDecl            { { $1 with statements  = $2::$1.statements } }
    | decls fnDecl              { { $1 with functions   = $2::$1.functions  } }

varDecl:
      varType ID SEMI           { ($1, $2) }
    /* @TODO - Get assignments on instantiation working
    | varType ID ASSIGN expr SEMI  { DeclAssign($1, $2, $4) } */

varDeclList:
      /* nothing */             { [] }
    | varDeclList varDecl       { $2 :: $1 }

objDecl:
<<<<<<< HEAD
    OBJECT ID LBRACE varDeclList statementsList RBRACE
=======
    OBJECT ID LBRACE varDeclList fnDeclList RBRACE
>>>>>>> c0fed97e34c376c4d8d244c6b167da1cc5c5d000
    { { objName     = $2;
        objLocals   = List.rev $4;
        methods     = List.rev $5 }}

stmtDecl:
      expr SEMI                 { Expr $1 }
    | RETURN SEMI               { Return Noexpr }
    | RETURN expr SEMI          { Return $2 }
    | LBRACE statementsList RBRACE   
        { Block(List.rev $2) }
    | IF LPAREN expr RPAREN stmtDecl %prec NOELSE 
        { If($3, $5, Block([])) }
    | IF LPAREN expr RPAREN stmtDecl ELSE stmtDecl    
        { If($3, $5, $7) }
    | FOR LPAREN optionalExpr SEMI expr SEMI optionalExpr RPAREN stmtDecl
        { For($3, $5, $7, $9) }
<<<<<<< HEAD
    | WHILE LPAREN expr RPAREN stmtDecl 
=======
    | WHILE LPAREN expr RPAREN stmtDecl
>>>>>>> c0fed97e34c376c4d8d244c6b167da1cc5c5d000
        { While($3, $5) }

statementsList:
      /* nothing */             { [] }
    | statementsList stmtDecl   { $2 :: $1 }

fnDecl:
    varType ID LPAREN optionalParameters RPAREN LBRACE varDeclList statementsList RBRACE
    { { returnType  = $1;
        fnName      = $2;
        parameters  = $4;
        fnLocals    = List.rev $7;
        body        = List.rev $8 } }

<<<<<<< HEAD
=======
fnDeclList:
    /* nothing */               { [] }
    | fnDeclList fnDecl         { $2 :: $1}

>>>>>>> c0fed97e34c376c4d8d244c6b167da1cc5c5d000
optionalParameters:
      /* nothing */             { [] }
    | parametersList            { List.rev $1 }

parametersList:
      varType ID                { [($1,$2)] }
    | parametersList COMMA varType ID  
        { ($3,$4) :: $1 }

varType:
      NUM                       { Num }
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

optionalExpr:
      /* nothing */             { Noexpr }
    | expr                      { $1 }

expr:
      LITERAL                   { Literal($1) }
    | STRLIT                    { StringLit($1) }
    | TRUE                      { BoolLit(true) }
    | FALSE                     { BoolLit(false) }
    | ID                        { Id($1) }

    | ID LBRACKET expr RBRACKET { Arrop($1, $3) }
<<<<<<< HEAD
<<<<<<< HEAD
    | ID DOT ID        { ObjLit($1, $3) }
    | ID DOT ID LPAREN actuals_opt RPAREN { ObjCall($1, $3, $5) }    

    | expr PLUS   expr { Binop($1, Add,   $3) }
    | expr MINUS  expr { Binop($1, Sub,   $3) }
    | expr TIMES  expr { Binop($1, Mult,  $3) }
    | expr DIVIDE expr { Binop($1, Div,   $3) }
    | expr EQ     expr { Binop($1, Equal, $3) }
    | expr NEQ    expr { Binop($1, Neq,   $3) }
    | expr LT     expr { Binop($1, Less,  $3) }
    | expr LEQ    expr { Binop($1, Leq,   $3) }
    | expr GT     expr { Binop($1, Greater, $3) }
    | expr GEQ    expr { Binop($1, Geq,   $3) }
    | expr AND    expr { Binop($1, And,   $3) }
    | expr OR     expr { Binop($1, Or,    $3) }
    | MINUS expr %prec NEG { Unop(Neg, $2) }
    | NOT expr         { Unop(Not, $2) }
    | ID ASSIGN expr   { Assign($1, $3) }
    | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
    | LPAREN expr RPAREN { $2 }

actuals_opt:
      /* nothing */ { [] }
    | actuals_list  { List.rev $1 }

actuals_list:
     expr                    { [$1] }
    | actuals_list COMMA expr { $3 :: $1 }
=======
=======
>>>>>>> c0fed97e34c376c4d8d244c6b167da1cc5c5d000
    | ID DOT ID                 { ObjLit($1, $3) }
    | ID DOT ID LPAREN optionalActuals RPAREN 
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
    | ID LPAREN optionalActuals RPAREN 
        { Call($1, $3) }
    | LPAREN expr RPAREN        { $2 }

optionalActuals:
      /* nothing */             { [] }
    | actualsList               { List.rev $1 }

actualsList:
      expr                      { [$1] }
<<<<<<< HEAD
    | actualsList COMMA expr    { $3 :: $1 }
>>>>>>> fc75dadf11640f601a554a9d7f09835fd42a7964
=======
    | actualsList COMMA expr    { $3 :: $1 }
>>>>>>> c0fed97e34c376c4d8d244c6b167da1cc5c5d000

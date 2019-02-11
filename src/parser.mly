%{
open Rexp
open Value

exception Invalid_stmts of Rexp.t
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token PLUS MINUS TIMES DIV MOD EXP
%token EQUAL
%token EQ NEQ LT LE GT GE ANDAND OROR
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token <string> IDENTIFY
%token NIL FALSE TRUE
%token IF THEN ELSE ELSIF END WHILE DEF BEGIN
%token FATARROW
%token SEMICOLON COMMA EOL
%token EOF

%left OROR
%left ANDAND
%left EQ NEQ
%left LT LE GT GE
%left PLUS MINUS
%left TIMES DIV MOD
%left EXP
%nonassoc UNARY

%start main

%type <Rexp.t> main

%%

main:
    compstmt EOF { $1 }
;

compstmt:
      stmts                { $1 }
    | blank_lines compstmt { $2 }
;

stmts:
      expr                   { Stmts [$1] }
    | expr blank_lines stmts { match $3 with Stmts ss -> Stmts ($1 :: ss) | other -> raise (Invalid_stmts other) }
    | expr blank_lines       { Stmts [$1] }
;

blank_lines:
      EOL       {}
    | SEMICOLON {}
    | EOL blank_lines       {}
    | SEMICOLON blank_lines {}
;

expr:
      INT    { Lit (Int $1) }
    | FLOAT  { Lit (Float $1) }
    | STRING { Lit (String $1) }
    | NIL    { Lit Nil }
    | FALSE  { Lit False }
    | TRUE   { Lit True }

    | PLUS expr %prec UNARY  { FuncCall ("+", [$2]) }
    | MINUS expr %prec UNARY { FuncCall ("-", [$2]) }

    | expr PLUS expr  { FuncCall ("+", [$1; $3]) }
    | expr MINUS expr { FuncCall ("-", [$1; $3]) }
    | expr TIMES expr { FuncCall ("*", [$1; $3]) }
    | expr DIV expr   { FuncCall ("/", [$1; $3]) }
    | expr MOD expr   { FuncCall ("%", [$1; $3]) }
    | expr EXP expr   { FuncCall ("**", [$1; $3]) }

    | expr EQ expr  { FuncCall ("==", [$1; $3]) }
    | expr NEQ expr { FuncCall ("!=", [$1; $3]) }
    | expr LT expr  { FuncCall ("<", [$1; $3]) }
    | expr LE expr  { FuncCall ("<=", [$1; $3]) }
    | expr GT expr  { FuncCall (">", [$1; $3]) }
    | expr GE expr  { FuncCall (">=", [$1; $3]) }

    | expr ANDAND expr { And ($1, $3) }
    | expr OROR expr   { Or ($1, $3) }

    | IDENTIFY EQUAL expr { VarAssign ($1, $3) }
    | IDENTIFY            { VarRef $1 }

    | IF expr then_expr elsif_end { If ($2, $3, $4) }

    | WHILE expr compstmt END { While ($2, $3) }

    | LBRACKET args RBRACKET { AryNew $2 }

    | expr LBRACKET expr RBRACKET            { AryRef ($1, $3) }
    | expr LBRACKET expr RBRACKET EQUAL expr { AryAssign ($1, $3, $6) }

    | LBRACE arrows RBRACE { HashNew $2 }

    | DEF IDENTIFY LPAREN params RPAREN compstmt END { FuncDef ($2, $4, $6) }

    | IDENTIFY LPAREN args RPAREN  { FuncCall ($1, $3) }

    | LPAREN expr RPAREN { $2 }
;

params:
                            { [] }
    | IDENTIFY              { [$1] }
    | IDENTIFY COMMA params { $1 :: $3 }
;

args:
                      { [] }
    | expr            { [$1] }
    | expr COMMA args { $1 :: $3 }
;

arrows:
                                      { [] }
    | expr FATARROW expr              { [$1; $3] }
    | expr FATARROW expr COMMA arrows { $1 :: $3 :: $5 }
;

elsif_end:
      END                            { Lit (Nil) }
    | ELSE compstmt END              { $2 }
    | ELSIF expr then_expr elsif_end { If ($2, $3, $4) }
;

then_expr:
      THEN compstmt        { $2 }
    | blank_lines compstmt { $2 }
;
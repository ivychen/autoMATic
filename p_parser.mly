%{ open P_ast %}

%token INCLUDE DEFINE UNDEF /* IF IFDEF IFNDEF ENDIF */
%token ENDL
%token WHITE
%token <string> VAR
%token <string> STRLIT
%token <int> INTLIT
%token <string> PASS
%token EOF

%start program
%type <P_ast.program> program

%left INCLUDE DEFINE UNDEF


%%

program:
| stmts EOF                         { $1 }

stmts:
| /* nothing */                     { [] }
| stmts stmt                        { $2 :: $1 }
| stmt                              { [$1] }

stmt:
| passthrough                       { $1 }
| VAR                               { Var($1) }
| INCLUDE ws STRLIT ws ENDL         { Inc($3) }
| DEFINE ws VAR ws INTLIT ws ENDL   { DefInt($3, $5) }
| DEFINE ws VAR ws VAR ws ENDL      { DefVar($3, $5) }
| DEFINE ws VAR ws STRLIT ws ENDL   { DefStrLit($3, $5) }
| UNDEF ws VAR ws ENDL              { Undef($3) }

passthrough:
| WHITE                             { Pass(" ") }
| ENDL                              { Pass("\n") }
| STRLIT                            { Pass("\"" ^ $1 ^ "\"") }
| INTLIT                            { Pass(string_of_int $1) }
| PASS                              { Pass($1) }

ws:
| /* nothing */                     {  }
| ws WHITE                          {  }
| WHITE                             {  }

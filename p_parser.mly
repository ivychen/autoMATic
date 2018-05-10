/* autoMATic Compiler - Spring 2018 - COMS4115 PLT
 by Ivy Chen ic2389, Nguyen Chi Dung ncd2118,
 Nelson Gomez ng2573, Jimmy O'Donnell jo2474 */

%{ open P_ast %}

%token INCLUDE DEFINE UNDEF IFDEF IFNDEF END
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
%right END

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
| INCLUDE w STRLIT w ENDL           { Inc($3) }
| DEFINE w VAR w INTLIT w ENDL      { DefInt($3, $5) }
| DEFINE w VAR w VAR w ENDL         { DefVar($3, $5) }
| DEFINE w VAR w STRLIT w ENDL      { DefStrLit($3, $5) }
| UNDEF w VAR w ENDL                { Undef($3) }
| IFDEF w VAR w ENDL                { Ifdef($3) }
| IFNDEF w VAR w ENDL               { Ifndef($3) }
| END w ENDL                        { End }

passthrough:
| WHITE                             { Pass(" ") }
| ENDL                              { Pass("\n") }
| STRLIT                            { Pass("\"" ^ $1 ^ "\"") }
| INTLIT                            { Pass(string_of_int $1) }
| PASS                              { Pass($1) }

w:
| /* nothing */                     {  }
| w WHITE                           {  }
| WHITE                             {  }

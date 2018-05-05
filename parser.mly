/* Ocamlyacc parser for autoMATic */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET PIPE
%token PLUS PLUSPLUS MINUS MINUSMINUS TIMES EXP ELEMTIMES DIVIDE ELEMDIVIDE TRANSPOSE MOD SLICE DOT ASSIGN AUTODECL
%token NOT EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE BREAK CONTINUE
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID MATRIX STRING
/* %token ARRAY */
%token AUTO
%token <int> LITERAL
%token <float> FLIT
%token <bool> BLIT
%token <string> ID
%token <string> STRLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%right AUTODECL
%left COMMA
%nonassoc SLICE
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES ELEMTIMES DIVIDE ELEMDIVIDE MOD
%left EXP
%right NOT NEG
%left PLUSPLUS MINUSMINUS TRANSPOSE


%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

vdecl:
   typ ID SEMI { ($1, $2) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
         fwasauto = ($1 = Auto);
      	 fname = $2;
      	 formals = $4;
      	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT                { Int   }
  | BOOL               { Bool  }
  | FLOAT              { Float }
  | STRING             { String }
  | VOID               { Void  }
  | matrix_type        { $1 }
  | AUTO               { Auto }
  | matrix_ret         { $1 }

/* Matrices are declared with the type its elements, and number of rows, columns
   Example: int matrix [3][3] m;
*/
matrix_type:
  primitive MATRIX LBRACKET LITERAL RBRACKET LBRACKET LITERAL RBRACKET  { Matrix($1, $4, $7) }

/* Return matrix from function, essentially a syntactic style choice
   Example: int matrix main() { ... }
 */
matrix_ret:
  primitive MATRIX    { MatrixRet($1)}

/* Primitive data types */
primitive:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | STRING { String }
  /* | MATRIX { Matrix } */
  | VOID  { Void  }
  /* | AUTO { Auto } */
  /* | ARRAY { Array } */

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    typ ID SEMI                             { VDecl($1, $2, Noexpr) }
  | typ ID ASSIGN expr SEMI                 { VDecl($1, $2, $4)     }
  | ID AUTODECL expr SEMI                   { VDecl(Auto, $1, $3)   }
  | expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | CONTINUE SEMI                           { Continue              }
  | BREAK SEMI                              { Break 1               }
  | BREAK LITERAL SEMI                      { Break $2              }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL              { IntLit($1)               }
  | FLIT	               { FloatLit($1)             }
  | TRUE                 { BoolLit(true)            }
  | FALSE                { BoolLit(false)           }
  | STRLIT               { StrLit($1)               }
  | ID                   { Id($1)                   }
  | expr PLUS       expr { Binop($1, Add,      $3)  }
  | expr MINUS      expr { Binop($1, Sub,      $3)  }
  | expr TIMES      expr { Binop($1, Mult,     $3)  }
  | expr EXP        expr { Binop($1, Exp,      $3)  }
  | expr ELEMTIMES  expr { Binop($1, ElemMult, $3)  }
  | expr DIVIDE     expr { Binop($1, Div,      $3)  }
  | expr ELEMDIVIDE expr { Binop($1, ElemDiv,  $3)  }
  | expr MOD        expr { Binop($1, Mod,      $3)  }
  | expr EQ         expr { Binop($1, Equal,    $3)  }
  | expr NEQ        expr { Binop($1, Neq,      $3)  }
  | expr LT         expr { Binop($1, Less,     $3)  }
  | expr LEQ        expr { Binop($1, Leq,      $3)  }
  | expr GT         expr { Binop($1, Greater,  $3)  }
  | expr GEQ        expr { Binop($1, Geq,      $3)  }
  | expr AND        expr { Binop($1, And,      $3)  }
  | expr OR         expr { Binop($1, Or,       $3)  }
  | MINUS expr %prec NEG { Unop(Neg, $2)            }
  | NOT expr             { Unop(Not, $2)            }
  | expr PLUSPLUS        { Unop(Inc, $1)            }
  | expr MINUSMINUS      { Unop(Dec, $1)            }
  | expr TRANSPOSE       { Unop(Trans, $1)          }
  | ID ASSIGN expr       { Assign($1, $3)           }
  | ID LPAREN args_opt RPAREN { Call($1, $3)        }
  | LPAREN expr RPAREN   { $2                       }
  /* Array literal declarations */
  /* | LBRACE PIPE args_opt PIPE RBRACE { ArrLit(List.rev $3)    }
  | ID LBRACKET expr RBRACKET { ArrAccess($1,$3)    } */

  /* Parsing explicit matrix declarations (matrix literals) */
  | LBRACKET mat_rows RBRACKET { MatLit(List.rev $2) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { MatAccess($1, $3, $6) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr { MatAssign($1, $3, $6, $9) }

mat_rows:
  | LBRACKET args_opt RBRACKET { [$2]               }
  | mat_rows SEMI LBRACKET args_opt RBRACKET { $4 :: $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

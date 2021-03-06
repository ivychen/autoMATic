(* autoMATic Compiler - Spring 2018 - COMS4115 PLT
 by Ivy Chen ic2389, Nguyen Chi Dung ncd2118,
 Nelson Gomez ng2573, Jimmy O'Donnell jo2474 *)

(* Ocamllex scanner for autoMATic *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
(* ---------- COMMENTS ----------- *)
| "/*"     { comment lexbuf }           (* Multiline Comments *)
| "//"     { linecomment lexbuf }       (* Singleline Comments *)
(* ---------- SYNTAX  ------------ *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '|'      { PIPE }
(* ---------- OPERATORS ---------- *)
| '+'      { PLUS }
| "++"     { PLUSPLUS }
| '-'      { MINUS }
| "--"     { MINUSMINUS }
| '*'      { TIMES }
| "**"     { EXP }
| ".*"     { ELEMTIMES }
| '/'      { DIVIDE }
| "./"     { ELEMDIVIDE }
| "'"      { TRANSPOSE }
| '%'      { MOD }
| ':'      { SLICE }
| ":="     { AUTODECL }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "not"    { NOT }
| '.'      { DOT }
(* ---------- CONTROL FLOW -------- *)
| "and"    { AND }
| "or"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "break"  { BREAK }
| "continue" { CONTINUE }
(* ---------- TYPES ---------- *)
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "matrix" { MATRIX }
| "string" { STRING }
| "void"   { VOID }
| "auto"   { AUTO }
| "fun"    { AUTO }
(* "array"  { ARRAY } *)
(* ---------- LITERALS -----------*)
| "true"   { TRUE }
| "false"  { FALSE }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm {
    FLIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| '"' ([^ '"']* as lxm) '"' { STRLIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and linecomment  = parse
 ['\n' '\r'] { token lexbuf }
| _         { linecomment lexbuf }

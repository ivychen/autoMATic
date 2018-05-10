(* autoMATic Compiler - Spring 2018 - COMS4115 PLT
 by Ivy Chen ic2389, Nguyen Chi Dung ncd2118,
 Nelson Gomez ng2573, Jimmy O'Donnell jo2474 *)

type stmt = 
    Pass of string
  | Inc of string
  | Var of string
  | DefInt of string * int 
  | DefVar of string * string
  | DefStrLit of string * string
  | Undef of string
  | Ifdef of string
  | Ifndef of string
  | End

type program = stmt list

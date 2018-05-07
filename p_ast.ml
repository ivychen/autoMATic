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

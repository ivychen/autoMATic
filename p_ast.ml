type stmt = 
    Inc of string
  | Var of string
  | DefInt of string * int 
  | DefVar of string * string
  | DefStrLit of string * string
  | Undef of string
  | Pass of string

type program = stmt list

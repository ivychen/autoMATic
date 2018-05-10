(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Exp | ElemMult | Div | ElemDiv | Mod | Equal | Neq
        | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not | Inc | Dec | Trans

(* type primitive = Int | Bool | Float | String | Void *)

type typ =
    Matrix of typ * int * int
  | Auto
  | Int
  | Bool
  | Float
  | String
  | Void

type expr =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | StrLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  (* Matrix specific *)
  | MatLit of expr list list
  | MatAccess of string * expr * expr
  | MatAssign of string * expr * expr * expr
  (* NO MORE ARRAYS *)
  (*  | ArrLit of expr list*)
  (* | ArrLit of expr list *)
  (* | ArrAccess of string * expr *)
  | Noexpr

type bind = typ * string

type stmt =
    Block of stmt list
  | VDeclList of typ * (string * expr) list
  | VDecl of typ * string * expr
  | Expr of expr
  | Continue
  | Break of int
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    mutable typ : typ;
    fwasauto: bool;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Exp -> "**"
  | ElemMult -> ".*"
  | Div -> "/"
  | ElemDiv -> "./"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"

let string_of_uop = function
    Neg -> "-"
  | Not -> "not"
  | Inc -> "++"
  | Dec -> "--"
  | Trans -> "'"

(* @TODO: (Ivy) Define slice operations *)

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | StrLit(s) -> s
  | MatLit(ll) -> "[" ^ String.concat ";" (List.map (fun lst -> "[" ^
                        String.concat "," (List.map string_of_expr lst) ^ "]") ll) ^ "]"
  | MatAccess(id, e1, e2) -> id ^ "[" ^ string_of_expr e1 ^ "][" ^ string_of_expr e2 ^ "]"
  | MatAssign(id, e1, e2, e3) -> id ^ "[" ^ string_of_expr e1 ^ "][" ^
                                 string_of_expr e2 ^ "] = " ^ string_of_expr e3
  (* | ArrLit(lst) -> "{|" ^ String.concat "," (List.map string_of_expr (List.rev lst)) ^ "|}" *)
  (* | ArrAccess(id, e1) -> id ^ "[" ^ string_of_expr e1 ^ "]" *)
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  (* | Matrix -> "matrix" *)
  (* | TMatrix(t) -> string_of_typ t ^ " matrix" *)
  | Void -> "void"
    (* DataType(Int)     -> "int"
  | DataType(Float)   -> "float"
  | DataType(String)  -> "string"
  | DataType(Bool)    -> "bool"
  | DataType(Void)    -> "void" *)
  | Matrix(t, r, c)   -> (match t with
        Int   -> "int matrix (r:" ^ string_of_int r ^ ", c:" ^ string_of_int c ^")"
      | Float -> "float matrix (r:" ^ string_of_int r ^ ", c:" ^ string_of_int c ^")"
      | Bool  -> "bool matrix (r:" ^ string_of_int r ^ ", c:" ^ string_of_int c ^")"
      | String -> "string matrix (r:" ^ string_of_int r ^ ", c:" ^ string_of_int c ^")"
      | Void  -> "void matrix (r:" ^ string_of_int r ^ ", c:" ^ string_of_int c ^")"
      | Matrix(_,_,_) -> "invalid"
      | Auto -> "invalid"
    )
  | Auto -> "auto"
  (* | Array -> "array" *)

let string_of_vdecl_list (n, e) =
  let suffix ex =
    if ex = Noexpr then ""
    else " = " ^ (string_of_expr ex)
  in n ^ (suffix e)

let string_of_bind (t, n) = string_of_typ t ^ " " ^ n ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Break(n) -> "break " ^ string_of_int n ^ ";\n";
  | Continue -> "continue;\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | VDecl(t, n, e) ->
      string_of_typ t ^ " " ^ n ^
      (if e = Noexpr then "" else " = " ^ string_of_expr e) ^ ";\n"
  | VDeclList(t, decls) ->
      string_of_typ t ^ " " ^ String.concat ", " (List.map string_of_vdecl_list decls) ^ ";\n"

let string_of_tuple x = "(" ^ (fst x) ^ " : " ^ string_of_typ (snd x) ^ ")"

(* Print out argument type and argument identifier *)
let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (
    List.map string_of_tuple (
      List.combine (List.map snd fdecl.formals) (List.map fst fdecl.formals)
      )
    ) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_bind vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

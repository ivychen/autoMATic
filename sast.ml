(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SStrLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  (* Matrix specific *)
  | SMatLit of sexpr list list
  | SMatAccess of string * sexpr * sexpr
  | SMatAssign of string * sexpr * sexpr * sexpr
  (*  | ArrLit of expr list *)
  (* | SArrLit of sexpr list
  | SArrAccess of string * sexpr *)
  | SNoexpr

type symtbl_entry = {
    ty : typ;
    (* reserved for qualifiers *)
  }

type blockent = {
    sparent : blockent option;
    symtbl  : (string, symtbl_entry) Hashtbl.t;
  }

type sstmt =
    SBlock of sstmt list * blockent
  | SVDecl of typ * string * sexpr
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sblockinfo : blockent;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntLit(l) -> string_of_int l
  | SFloatLit(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SStrLit(l) -> l
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SMatLit(ell) ->
      "[" ^ String.concat ", " (List.map (fun el ->
      "[" ^ String.concat ", " (List.map string_of_sexpr el)) ell) ^ "]"
  | SMatAccess(s, e1, e2) ->
      s ^ "[" ^ string_of_sexpr e1 ^ "][" ^ string_of_sexpr e2 ^ "]"
  | SMatAssign(s, e1, e2, e3) ->
      s ^ "[" ^ string_of_sexpr e1 ^ "][" ^ string_of_sexpr e2 ^ "] = " ^
      string_of_sexpr e3
  (* | SArrLit(el) ->
      "{" ^ String.concat ", " (List.map string_of_sexpr el) ^ "}"
  | SArrAccess(s, e) ->
      s ^ "[" ^ string_of_sexpr e ^ "]" *)
  | SNoexpr -> ""
				  ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts, _) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([], _)) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SVDecl(t, n, e) ->
      string_of_typ t ^ " " ^ n ^
      (if (snd e) = SNoexpr then "" else " = " ^ string_of_sexpr e) ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)

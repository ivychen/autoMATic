(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  let glob_symtbl = Hashtbl.create 10 in
  let check_binds (kind : string) (to_check : bind list) tbl = 
    let check_it binding = 
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (t, n1) -> (* No duplicate bindings *)
                    if Hashtbl.mem tbl n1
                    then raise (Failure dup_err)
                    else let entry = {
                        ty = t;
                    }
                    in Hashtbl.add tbl n1 entry
    in let _ = List.iter check_it (List.sort compare to_check) 
       in to_check
  in 

  (**** Checking Global Variables ****)
  let globals' = check_binds "global" globals glob_symtbl in

  (* Create the global symtbl block that functions inherit from. *)
  let glob_block = {
      sparent = None;
      symtbl = glob_symtbl;
  }
  in

  (**** Checking Functions ****)


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, argtypes, returntype) = StringMap.add name {
      typ = returntype; fname = name;
      formals = List.mapi (fun idx argtype -> (argtype, "x" ^ string_of_int idx)) argtypes;
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("printstr", [String], Void);
                                                 ("print", [Int], Void);
                                                 ("size", [Matrix], Matrix);
                                                 ("det", [Matrix], Float);
                                                 ("minor", [Matrix; Int; Int], Matrix);
                                                 ("inv", [Matrix], Matrix);
                                                 ("tr", [Matrix], Float)]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function_body func =
    (* Create a new block for this function *)
    let funblk = {
      sparent = Some glob_block;
      symtbl = Hashtbl.copy glob_block.symtbl;
    }
    in

    (* Make sure no formals are void or duplicates *)
    let formals' = check_binds "formal" func.formals funblk.symtbl in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s tbl =
      let entry = (if Hashtbl.mem tbl s
                   then Hashtbl.find tbl s
                   else raise (Failure ("undeclared identifier " ^ s)))
      in entry.ty
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr blk = function
        IntLit   l -> (Int, SIntLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | BoolLit  l -> (Bool, SBoolLit l)
      | StrLit   l -> (String, SStrLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s blk.symtbl, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var blk.symtbl
          and (rt, e') = expr blk e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr blk e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr blk e1 
          and (t2, e2') = expr blk e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr blk e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr blk e = 
      let (t', e') = expr blk e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt (blk : blockent) x =
        let _ = print_string ((string_of_stmt x) ^ "\n\n") in
        match x with
        Expr e -> SExpr (expr blk e)
      | VDecl(t, n, e) as st ->
          let (et, e') = expr blk e in
          let redecl_err = "conflicting variable declaration " ^ n ^ " in function " ^ func.fname
          and type_err = "illegal variable instantiation " ^ string_of_typ t ^ " = " ^
            string_of_typ et ^ " in " ^ string_of_stmt st in
          let entry = {
            ty = t;
          }
          in
          let _ = if Hashtbl.mem blk.symtbl n
                  then raise (Failure redecl_err)
                  else Hashtbl.add blk.symtbl n entry
          in
          if e' = SNoexpr
          then SVDecl(t, n, (et, e'))
          else SVDecl((check_assign t et type_err), n, (et, e'))
      | If(p, b1, b2) -> SIf(check_bool_expr blk p, check_stmt blk b1, check_stmt blk b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr blk e1, check_bool_expr blk e2, expr blk e3, check_stmt blk st)
      | While(p, s) -> SWhile(check_bool_expr blk p, check_stmt blk s)
      | Return e -> let (t, e') = expr blk e in
        if func.typ = Auto then func.typ <- t;
        
        if t = func.typ then SReturn (func.typ, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt blk s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt blk s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl, blk)

    in let blk = check_stmt funblk (Block func.body)
       and err = "internal error: block didn't become a block?"

    in let get_block_sl b = match b with
        SBlock(sl, _) -> sl
      | _ -> raise (Failure err)
    
    and get_block_bi b = match b with
        SBlock(_, bi) -> bi
      | _ -> raise (Failure err)
    
    in (* body of check_function_body *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody = get_block_sl blk;
      sblockinfo = get_block_bi blk;
    }

  (* TODO: Use this for resolving types of any auto-decl functions *)
  (*in let resolve_auto_decl = *)

  in (globals', List.map check_function_body functions)

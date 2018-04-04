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
  let glob_symtbl = Hashtbl.create 20 in
  let check_binds (kind : string) (to_check : bind list) = 
    let check_it binding = 
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (t, n1) -> (* No duplicate bindings *)
                    if Hashtbl.mem glob_symtbl n1
                    then raise (Failure dup_err)
                    else let entry = {
                        ty = t;
                    }
                    in Hashtbl.add glob_symtbl n1 entry
    in let _ = List.iter check_it (List.sort compare to_check) 
       in to_check
  in 

  (**** Checking Global Variables ****)
  let globals' = check_binds "global" globals in

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

  let check_function_body (pb : blockent) func =
    (* Make sure no formals are void or duplicates *)
    let formals' = check_binds "formal" func.formals in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Add formal parameters to the symbol hashtable *)
    let add_formal_to_symtbl (x : bind) =
        let err = "formal " ^ (snd x) ^ " in func " ^ func.fname ^
            "conflicts with a global variable binding"
        and entry = {
            ty = fst x;
        }
        in if Hashtbl.mem pb.symtbl (snd x)
        then raise (Failure err)
        else Hashtbl.add pb.symtbl (snd x) entry
    in

    let _ = List.iter add_formal_to_symtbl formals'
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      let entry = (if Hashtbl.mem pb.symtbl s
                   then Hashtbl.find pb.symtbl s
                   else raise (Failure ("undeclared identifier " ^ s)))
      in entry.ty
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        IntLit   l -> (Int, SIntLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | BoolLit  l -> (Bool, SBoolLit l)
      | StrLit   l -> (String, SStrLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
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
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt (blkinfo : blockent) = function
        Expr e -> SExpr (expr e)
      | VDecl(t, n, e) as st ->
          let (et, e') = expr e in
          let redecl_err = "conflicting variable declaration " ^ n ^ " in function " ^ func.fname
          and type_err = "illegal variable instantiation " ^ string_of_typ t ^ " = " ^
            string_of_typ et ^ " in " ^ string_of_stmt st in
          let _ = if Hashtbl.mem pb.symtbl n then raise (Failure redecl_err) in
          SVDecl((check_assign t et type_err), n, (et, e'))
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt blkinfo b1, check_stmt blkinfo b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt blkinfo st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt blkinfo s)
      | Return e -> let (t, e') = expr e in
        if func.typ = Auto then func.typ <- t;
        
        if t = func.typ then SReturn (func.typ, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt blkinfo s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt blkinfo s :: check_stmt_list ss
            | []              -> []
          in let newblk = {
            sparent = Some blkinfo;
            symtbl = Hashtbl.copy blkinfo.symtbl;
          }
          in SBlock(check_stmt_list sl, newblk)

    in let blk = check_stmt pb (Block func.body)
    in let err = "internal error: block didn't become a block?"
    in let get_block_sl b = match b with
        SBlock(sl, _) -> sl
      | _ -> raise (Failure err)
    
    in let get_block_bi b = match b with
        SBlock(_, bi) -> bi
      | _ -> raise (Failure err)
    
    in (* body of check_function_body *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody = get_block_sl blk;
      sblockinfo = get_block_bi blk;
    }

  (* When checking function declarations, start symbol table hierarchy from the global block. *)
  in let check_function x = check_function_body glob_block x

  (* TODO: Use this for resolving types of any auto-decl functions *)
  (*in let resolve_auto_decl = *)

  in (globals', List.map check_function functions)

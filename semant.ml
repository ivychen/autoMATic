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
      and auto_err = "illegal auto " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
        (* No auto bindings *)
      | (Auto, _) -> raise (Failure auto_err)
      | (t, n1) -> (* No duplicate bindings *)
                    if Hashtbl.mem tbl n1
                    then raise (Failure dup_err)
                    else let entry = {
                        ty = t;
                        ety = None;
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
      typ = returntype; fwasauto = false; fname = name;
      formals = List.mapi (fun idx argtype -> (argtype, "x" ^ string_of_int idx)) argtypes;
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("printstr", [String], Void);
                                                 ("print", [Int], Void);
                                                 ("size", [MatrixRet(Int)], Matrix(Int,1,2));
                                                 (* ("det", [Matrix], DataType(Float)); *)
                                                 ("printflt", [Float], Void);
                                                 (* ("size", [Matrix], Matrix);
                                                 ("minor", [Matrix; Int; Int], Matrix);
                                                 ("inv", [Matrix], Matrix);
                                                 ("tr", [Matrix], Float)] *) ];
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

  let rec check_function (func : func_decl) : sfunc_decl =
    (* Create a new block for this function *)
    let funblk = {
      sparent = Some glob_block;
      symtbl = Hashtbl.copy glob_block.symtbl;
    }
    in

    (* Keeps track of levels of loop nesting for break/continue statements *)
    let loop_depth = ref 0 in

    (* Make sure no formals are void or duplicates *)
    let formals' = check_binds "formal" func.formals funblk.symtbl in

    (* Matrix checking helpers *)
    (* Check if param is a matrix or matrix shorthand *)
    let is_mat m = match m with
      Matrix(_,_,_) | MatrixRet(_) -> true
    | _ -> false
    in
    (* Return element type of matrix or matrix shorthand *)
    let mat_typ m = match m with
      Matrix(t, _, _) | MatrixRet(t) -> t
    | _ -> Void
    in
    (* Return matrix dimensions (row, col) *)
    let mat_dim m = match m with
      Matrix(_, r, c) -> (r, c)
    | _ -> (0, 0)
    in

    (* let data_typ d = match d with
      t -> t
    | _ -> Void
    in *)
    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       let mat_eq a b = if (mat_typ a = mat_typ b) && (mat_dim a = mat_dim b) then true else false
       in
       (* both sides are matrices with same type and equal dimension *)
       if is_mat lvaluet && is_mat rvaluet && mat_eq lvaluet rvaluet then lvaluet
       (* LHS is non-matrix, RHS is matrix -> LHS *)
       else if not (is_mat lvaluet) && is_mat rvaluet && (lvaluet = mat_typ rvaluet) then lvaluet
       (* LHS is matrix, RHS is non-matrix -> LHS *)
       else if is_mat lvaluet && not (is_mat rvaluet) && (lvaluet = mat_typ rvaluet) then lvaluet
       else if lvaluet = rvaluet then lvaluet
       else raise (Failure err)
    in

    (* Return a variable from our local symbol table *)
    (* entry.ty is one of:
          Matrix(primitive, int, int)
          Auto
          DataType(primitive)
          MatrixRet(primitive)
    *)
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
          (* If assigning matrix literal, update the symbtbl entry for the matrix element type (ety) field *)
          (* if (is_mat lt && is_matt rt) then
            let _ = print_string "okay updating type" in
            let entry = {
              ty = type_of_identifier var blk.symtbl;
              ety = ety';
            }
            in
            let _ = if Hashtbl.mem blk.symtbl var
                    then Hashtbl.add blk.symtbl var entry
                    else raise(Failure ("undeclared identifier " ^ var))
            in *)
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr blk e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          (* Increment++/Decrement-- : only work on Int or Int Matrix *)
          | Inc when t = Int -> t
          | Dec when t = Int -> t
          (* | Inc when t = Int || ((is_mat t) && (mat_typ t) = Int) -> t *)
          (* Tranpose only works on matix *)
          | Trans when (is_mat t) -> t
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
            Add | Sub | Mult | Div | Mod | Exp when same && t1 = Int      -> Int
          | Add | Sub | Mult | Div | Mod | Exp when same && t1 = Float    -> Float
          | Exp when (t1 = Int && t2 = Float) || (t1 = Float && t2 = Int) -> Float
          | Equal | Neq                        when same                  -> Bool
          (* Matrix Addition/Subtraction/ElemDiv/ElemMult require matrices of the same dimensions and type *)
          | Add | Sub | ElemDiv | ELemMult when (is_mat t1 && is_mat t2 && (mat_dim t1 = mat_dim t2) && (mat_typ t1 = mat_typ t2))  -> t1
          (* Matrix multiplication requires matrices of the same inner dimensions and type *)
          | Mult when (is_mat t1 && is_mat t2 && (mat_typ t1 = mat_typ t2) && ((snd mat_dim t1) = (fst mat_dim t2))) -> Matrix(mat_typ t1, fst (mat_dim t1), snd (mat_dim t2))
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
        Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      (* === Check: Matrix literals, Matrix Accessor, Matrix Assignment *)
      | MatLit(m) as mat ->
      (* Check: MatLit (matrix literal declarations)*)
        (* Check matrix elements and construct list of sexpr *)
        let check_mat_elem ls elem =
          (* Semantically check list elements *)
          let selem = expr blk elem in
          match ls with
              []        ->  (List.append ls [selem])
            | hd :: _   ->  let ty1 = fst (hd) in
                            let ty2 = fst (selem) in
                            if ty1 != ty2 then raise (Failure("Matrix element types are inconsistent: " ^ string_of_typ ty1 ^ " with " ^ string_of_typ ty2 ^ " in " ^ string_of_expr mat)) else (List.append ls [selem])
          in
        (* Check matrix rows, add to matrix *)
        let check_mat_rows mtx ls =
          (* Semantically check lists *)
          let sls = List.fold_left check_mat_elem [] ls in
          match mtx with
              []      ->  (List.append mtx [sls])
            | [] :: _ ->
                if List.length sls = 0
                then List.append mtx [sls] else raise(Failure("Matrices may not be jagged in " ^ string_of_expr mat))
            | (hd :: _) :: _  ->
                if List.length (List.hd mtx) != List.length sls
                then raise(Failure("Matrices may not be jagged in " ^ string_of_expr mat))
                else
                  let ty1 = fst (hd) in
                    let ty2 = fst (List.hd sls) in
                    if ty1 != ty2
                    then raise (Failure("Matrix element types are inconsistent: " ^ string_of_typ ty1 ^ " with " ^ string_of_typ ty2 ^ " in " ^ string_of_expr mat))
                    else (List.append mtx [sls])
            in
          (* Semantically checked matrix *)
          let smat = List.fold_left check_mat_rows [] m in
          (* Type of matrix is type of its elements (uniform); restricted to Int, Bool, Float types*)
          let num_rows = List.length m in
          let num_cols = (if num_rows = 0 then 0 else List.length (List.hd m)) in
          (* Get matrix type *)
          let mty = fst (List.hd (List.hd smat)) in
            if mty = Int then (Matrix(Int, num_rows, num_cols), SMatLit(smat))
            else if mty = Float then (Matrix(Float, num_rows, num_cols), SMatLit(smat))
            else if mty = Bool then (Matrix(Bool, num_rows, num_cols), SMatLit(smat))
            else raise(Failure("Matrix elements must be of type Int, Bool or Float"))
            (* if mty != DataType(Int) && mty != DataType(Float) && mty != DataType(Bool)
            then raise(Failure("Matrix elements must be of type Int, Bool or Float"))
            else (Matrix(mty, num_rows, num_cols), SMatLit(smat)) *)
      | MatAccess(s,e1,e2) as ex ->
          let se1 = expr blk e1 in
          let se2 = expr blk e2 in
          let ty = type_of_identifier s blk.symtbl in
          (* Check that access indices are integers *)
          let _ = (match (fst se1) with
            Int -> Int
          | _ -> raise (Failure ("attempting to access with a non-integer type")))
          and _ = (match (fst se2) with
            Int -> Int
          | _ -> raise (Failure ("attempting to access with a non-integer type"))) in
          (* Semantically checked matrix assignment *)
          (match ty with
              Matrix(t, _, _) | MatrixRet(t) ->  (t, SMatAccess(s, se1, se2))
            | _       ->  raise(Failure("Cannot access elements of non-matrix type " ^ string_of_typ ty ^ " in " ^ string_of_expr ex)))

      | MatAssign(s,e1,e2,e3) as ex ->
          (* Left hand side *)
          let se1 = expr blk e1 in
          let se2 = expr blk e2 in
          (* Right hand side *)
          let se3 = expr blk e3 in
          (* Check that access indices are integers *)
          let _ = (match (fst se1) with
            Int -> Int
          | _ -> raise (Failure ("attempting to access with a non-integer type")))
          and _ = (match (fst se2) with
            Int -> Int
          | _ -> raise (Failure ("attempting to access with a non-integer type"))) in
          let extract_ty elem = (match elem with
            Int ->  Int
          | Float -> Float
          | Bool -> Bool
          | Matrix(p,_,_)   -> if p = Int then Int
                           else if p = Float then Float
                           else if p = Bool then Bool
                           else raise(Failure("Right-hand type is not Int, Bool or Float"))
          | _           -> raise(Failure("Invalid matrix assignment, right hand side not matrix/int/bool/float"))
          )
          in let rhs_ty = extract_ty (fst se3) in
          (* Matrix type *)
          let ty = type_of_identifier s blk.symtbl in
          (match ty with
              Matrix(t, _, _) | MatrixRet(t)  -> if t = rhs_ty then (t, SMatAssign(s, se1, se2, se3))
                                  else raise(Failure("Invalid matrix assignment"))
            | _       ->  raise(Failure("Cannot assign incompatible element of " ^ string_of_typ ty ^ " in " ^ string_of_expr ex)))

      | Call(fname, args) as call ->
          let fd = find_func fname in
          let _ = (if fd.typ = Auto then (let _ = check_function fd in fd) else fd) in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, n) e =
            let (et, e') = expr blk e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e in
            (* Auto declr param check *)
            let auto_err = "function " ^ fname ^
              " has illegal auto-declared parameter " ^ n
            (* Matrix param check *)
            in let ft' = if is_mat et && is_mat ft && (mat_typ et = mat_typ ft) then et else ft
            in let void_err = "illegal void formal " ^ n
            in let _ = if ft = Auto then raise (Failure auto_err)
            in let _ = if ft = Void then raise (Failure void_err)
            in (check_assign ft' et err, e')
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
    let rec check_stmt (blk : blockent) = function
        Expr e -> SExpr (expr blk e)
      | VDecl(t, n, e) as st ->
          let (et, e') = expr blk e in
          let redecl_err = "conflicting variable declaration " ^ n ^ " in function " ^ func.fname
          and type_err = "illegal variable instantiation " ^ string_of_typ t ^ " = " ^
            string_of_typ et ^ " in " ^ string_of_stmt st ^ " in function " ^ func.fname
          and auto_err = "declared auto variable without initializer in " ^
            string_of_stmt st ^ " in function " ^ func.fname


          (* Change type to RHS if auto *)
          in let _ = if t = Auto && e = Noexpr
                     then raise (Failure auto_err)

          in let t' = if t = Auto then et
                      else t

          in let entry = {
            ty = t';
            ety = None;
          }

          in
          let _ = if Hashtbl.mem blk.symtbl n
                  then raise (Failure redecl_err)
                  else Hashtbl.add blk.symtbl n entry
          in
          if e' = SNoexpr
          then SVDecl(t', n, (et, e'))
          else SVDecl((check_assign t' et type_err), n, (et, e'))
      | If(p, b1, b2) -> SIf(check_bool_expr blk p, check_stmt blk b1, check_stmt blk b2)
      | For(e1, e2, e3, st) ->
          let _ = loop_depth := !loop_depth + 1 in
    SFor(expr blk e1, check_bool_expr blk e2, expr blk e3, check_stmt blk st)
      | While(p, s) ->
          let _ = loop_depth := !loop_depth + 1 in
          SWhile(check_bool_expr blk p, check_stmt blk s)
      | Continue ->
        if !loop_depth = 0 then raise (Failure "Attempted to call continue without being in a loop")
        else SContinue
      | Break n ->
        if !loop_depth = 0 then raise (Failure "Attempted to break out of loop without being in a loop")
        else if n > !loop_depth then raise (Failure "Count on 'break' call too large for loop depth")
        else if n < 1 then raise (Failure "Cannot break out of less than one level of looping")
        else SBreak n
      | Return e -> let (t, e') = expr blk e in
        (* If function return type is AUto, update return type to binding *)
        if func.typ = Auto then func.typ <- t;
        (* If function return type is a matrix, update return to specific matrix binding *)
        if is_mat func.typ && is_mat t && (mat_typ func.typ = mat_typ t) then func.typ <- func.typ;
        (* If returning matrix, check if type match *)
        if is_mat func.typ && is_mat t && (mat_typ func.typ = mat_typ t) then SReturn(t, e')
        else if t = func.typ then SReturn(func.typ, e')
        else if func.fwasauto
             then raise (Failure ("function " ^ func.fname ^ " is declared auto but return type is ambiguous: returns both " ^ string_of_typ t ^ " and " ^ string_of_typ func.typ))
             else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^ string_of_typ func.typ ^ " in " ^ string_of_expr e))

      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt blk s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  ->
                let child_blk = {
                  sparent = Some blk;
                  symtbl = Hashtbl.copy blk.symtbl;
                }
                in check_stmt_list ss @ [check_stmt child_blk (Block sl)]
            | s :: ss         -> check_stmt_list ss @ [check_stmt blk s]
            | []              -> []
          in SBlock(List.rev (check_stmt_list sl), blk)

    in let blk = check_stmt funblk (Block func.body)
       and err = "internal error: block didn't become a block?"

    in let get_block_sl b = match b with
        SBlock(sl, _) -> sl
      | _ -> raise (Failure err)

    and get_block_bi b = match b with
        SBlock(_, bi) -> bi
      | _ -> raise (Failure err)

    in let _ = (if func.typ = Auto then func.typ <- Void);

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody = get_block_sl blk;
      sblockinfo = get_block_bi blk;
    }

  in (globals', List.map check_function functions)

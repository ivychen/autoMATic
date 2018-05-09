(* autoMATic Compiler - Spring 2018 - COMS4115 PLT
 by Ivy Chen ic2389, Nguyen Chi Dung ncd2118,
 Nelson Gomez ng2573, Jimmy O'Donnell jo2474 *)

(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

    http://llvm.moe/
    http://llvm.moe/ocaml/ *)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
    let context = L.global_context () in
    (* Create an LLVM module -- this is a "container" into which we'll
       generate actual code *)
    let the_module = L.create_module context "autoMATic" in
    (* Add types to the context so we can use them in our LLVM code *)
    let i32_t   = L.i32_type          context
    and i8_t    = L.i8_type           context
    and i1_t    = L.i1_type           context
    and float_t = L.double_type       context
    and void_t  = L.void_type         context in
    let str_t   = L.pointer_type i8_t
    and array_t = L.array_type
    and lint    = L.const_int i32_t
    and lfloat  = L.const_float float_t
    and zero    = L.const_int i32_t 0
    and one     = L.const_int i32_t 1
    and matrix_i   = L.named_struct_type context "matrix_t" in
                     (* L.struct_set_body matrix_i [|i32_t; i32_t; i32_t; i8_t; L.pointer_type i32_t|] false; *)
                     L.struct_set_body matrix_i [| i32_t; i32_t; i8_t; L.pointer_type i32_t |] false;
    let matrix_f   = L.named_struct_type context "matrix_t" in
                     L.struct_set_body matrix_f [| i32_t; i32_t; i8_t; L.pointer_type float_t|] false;
    let matrix_b   = L.named_struct_type context "matrix_t" in
                     L.struct_set_body matrix_b [| i32_t; i32_t; i8_t; L.pointer_type i1_t|] false;

(* === Helpers for autoMATic/LLVM types === *)
(* Convert autoMATic types to LLVM types *)
let ltype_of_typ = function
    | A.Int    -> i32_t
    | A.Bool   -> i1_t
    | A.Float  -> float_t
    | A.Void   -> void_t
    | A.String -> str_t
    | A.Matrix(typ, rows, cols) -> (match typ with
                                        | A.Int   -> array_t (array_t i32_t cols)   rows
                                        | A.Bool  -> array_t (array_t i1_t cols)    rows
                                        | A.Float -> array_t (array_t float_t cols) rows
                                        | _       -> raise (Failure "internal error: invalid matrix type"))
    | A.Auto   -> (raise (Failure "internal error: unresolved autodecl"))
    |_ -> raise (Failure "internal error: undefined type") in

(* Based on llvalue type, determine autoMATic type *)
let type_of_llvalue llval =
  let lltype_string = L.string_of_lltype llval in
  (match lltype_string with
    "void" -> A.Void
  | "i32"  -> A.Int
  | "double" -> A.Float
  | "i1"    -> A.Bool
  | "i8*"    -> A.String
  | _       -> raise (Failure "invalid type"))
in

let type_of_lvalue lv =
  let lltype = L.type_of lv in
  type_of_llvalue lltype
in

(* Initialization helper: function used to initialize global and local variables 
let empty_string = L.define_global "__empty_string" (L.const_stringz context "") the_module in
let init_var typ = (match typ with
                      A.Int   -> L.const_int i32_t 0
                    | A.Float -> L.const_float float_t 0.0
                    | A.Bool -> L.const_int i1_t 0
                    | A.String -> L.const_bitcast empty_string str_t
                    | A.Void -> L.const_null void_t
                    | A.Matrix(ty, _, _) -> (match ty with
                                              A.Int -> L.const_null matrix_i
                                            | A.Float -> L.const_null matrix_f
                                            | A.Bool  -> L.const_null matrix_b
                                            | _       -> raise (Failure "error: invalid matrix type"))
                    | _ -> L.const_int i32_t 0
                    ) 
in *)

(* Get corresponding llvm instruction for matrix operations *)
let bop_of = function
    | A.Add -> L.build_add 
    | A.Sub -> L.build_sub
    | A.And -> L.build_and
    | A.Or  -> L.build_or
    | _ -> raise (Invalid_argument "illegal operation on boolean matrices") 
in

let iop_of = function
    | A.Add      -> L.build_add
    | A.Sub      -> L.build_sub 
    | A.ElemMult -> L.build_mul
    | A.ElemDiv  -> L.build_sdiv
    | _ -> raise (Invalid_argument "illegal operation on integer matrices")
in

let fop_of = function
    | A.Add      -> L.build_fadd
    | A.Sub      -> L.build_fsub 
    | A.ElemMult -> L.build_fmul
    | A.ElemDiv  -> L.build_fdiv
    | _ -> raise (Invalid_argument "illegal operation on float matrices")
in

(* Declare each global variable; remember its value in a map *)
let global_vars =
    let global_var m (t, n) =
    let init = L.const_int (ltype_of_typ t) 0
    in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func = L.declare_function "printf" printf_t the_module in

let pow_t = L.function_type float_t [| float_t; float_t |] in
let pow_func = L.declare_function "pow" pow_t the_module in

(* let size_t = L.function_type (ltype_of_typ A.matrix) [| ltype_of_type A.Matrix |] in
let size_func = L.declare_function "size" size_t the_module in

let det_t = L.function_type float_t [| ltype_of_type A.Matrix |] in
let det_func = L.declare_function "det" det_t the_module in

let minor_t = L.function_type (ltype_of_type A.matrix) [| ltype_of_type A.Matrix; i32_t; i32_t |] in
let minor_func = L.declare_function "minor" minor_t the_module in

let inv_t = L.function_type (ltype_of_type A.matrix) [| ltype_of_type A.Matrix |] in
let inv_func = L.declare_function "inv" inv_t the_module in

let tr_t = L.function_type float_t [| ltype_of_type A.Matrix |] in
let tr_func = L.declare_function "tr" tr_t the_module in *)

(*let printbig_t = L.function_type i32_t [| i32_t |] in
let printbig_func = L.declare_function "printbig" printbig_t the_module in *)

(* Define each function (arguments and return type) so we can
   define it's body and call it later *)
let function_decls =
    let function_decl m fdecl =
        let name = fdecl.sfname
        and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals) in
        let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions in

(* Fill in the body of the given function *)
let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d" "fmt" builder
    and float_format_str = L.build_global_stringptr "%f" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s" "fmt" builder in

    let int_format_str_n = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str_n = L.build_global_stringptr "%f\n" "fmt" builder
    and string_format_str_n = L.build_global_stringptr "%s\n" "fmt" builder in


    (* Keep track of jump locations for breaking out of/continuing loops *)
    let continue_stack = ref []
    and break_stack = ref []
    in

    (* Allocate space for any locally declared variables and add the
     * resulting registers to our map *)
    let add_local m (t, n) builder =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in Hashtbl.add m n local_var
    in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars = Hashtbl.create 10
    in let add_formal m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
        Hashtbl.add m n local
        in
    let add_formal_to_locals x y = add_formal local_vars x y
    in let _ = List.iter2 add_formal_to_locals fdecl.sformals (Array.to_list (L.params the_function))
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try Hashtbl.find local_vars n
    with Not_found -> StringMap.find n global_vars
    in

    let rec expr builder (_, e) = match e with
    | SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SFloatLit l -> L.const_float float_t l
    | SStrLit s -> L.build_global_stringptr s "" builder
    | SNoexpr -> L.const_int i32_t 0
    | SId s -> L.build_load (lookup s) s builder
    | SBinop (e1, op, e2) ->
        let (t, _) = e1
        and e1' = expr builder e1
        and e2' = expr builder e2 in (match t with
        (* Binary bool operations *)
        | A.Bool -> (match op with
            | A.And     -> L.build_and e1' e2' "tmp" builder
            | A.Or      -> L.build_or e1' e2' "tmp" builder
            | A.Equal   -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
            | A.Neq     -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
            | _         -> raise (Failure "internal error: semant should have rejected and/or on float"))
        (* Binary float operations *)
        | A.Float -> (match op with
            | A.Add     -> L.build_fadd e1' e2' "tmp" builder
            | A.Sub     -> L.build_fsub e1' e2' "tmp" builder
            | A.Mult    -> 
                let (ty, _) = e2 in 
                if ty = A.Float then L.build_fmul e1' e2' "tmp" builder
                else let result = L.build_alloca (ltype_of_typ ty) "result" builder in 
                     let _ = L.build_store e2' result builder in (match ty with
                        | A.Matrix(A.Float, rows, cols) -> 
                            for i = 0 to rows - 1 do
                                for j = 0 to cols - 1 do
                                    let reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder in 
                                    let prod = L.build_fmul (L.build_load reg "load" builder) e1' "prod" builder 
                                    in ignore (L.build_store prod reg builder)
                                done;
                            done; L.build_load result "prod" builder
                        | _ -> raise (Invalid_argument "invalid scalar multiplication")
                     )
            | A.Exp     -> 
                let (ty, _) = e2 in
                let safe_cast = if ty = A.Int then L.build_sitofp e2' float_t "safe_cast" builder else e2'
                in L.build_call pow_func [| e1'; safe_cast |] "exp" builder
            | A.Div     -> L.build_fdiv e1' e2' "tmp" builder
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
            | A.Neq     -> L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
            | A.Less    -> L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
            | A.Leq     -> L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
            | A.Geq     -> L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
            | A.Mod     -> L.build_frem  e1' e2' "tmp" builder
            | _         -> raise (Failure "internal error: semant should have rejected and/or on float")
        )

        (* Binary integer operations *)
        | A.Int -> (match op with
            | A.Add     -> L.build_add e1' e2' "tmp" builder
            | A.Sub     -> L.build_sub e1' e2' "tmp" builder
            | A.Mult    -> 
                let (ty, _) = e2 in 
                if ty = A.Int then L.build_mul e1' e2' "tmp" builder
                else let result = L.build_alloca (ltype_of_typ ty) "result" builder in 
                     let _ = L.build_store e2' result builder in (match ty with
                        | A.Matrix(A.Int, rows, cols) -> 
                            for i = 0 to rows - 1 do
                                for j = 0 to cols - 1 do
                                    let reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder in 
                                    let prod = L.build_mul (L.build_load reg "load" builder) e1' "prod" builder 
                                    in ignore (L.build_store prod reg builder)
                                done;
                            done; L.build_load result "prod" builder
                        | _ -> raise (Invalid_argument "invalid scalar multiplication")
                     )
            | A.Exp     -> 
                let (ty, _) = e2 in
                let cast = L.build_sitofp e1' float_t "cast" builder
                and safe_cast = if ty = A.Float then e2' else L.build_sitofp e2' float_t "safe_cast" builder in
                let result = L.build_call pow_func [| cast; safe_cast |] "exp" builder in
                if ty = A.Int then L.build_fptosi result i32_t "result" builder else result
            | A.Div     -> L.build_sdiv e1' e2' "tmp" builder
            | A.And     -> L.build_and  e1' e2' "tmp" builder
            | A.Or      -> L.build_or   e1' e2' "tmp" builder
            | A.Equal   -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
            | A.Neq     -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
            | A.Less    -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
            | A.Leq     -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
            | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
            | A.Geq     -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
            | A.Mod     -> L.build_srem  e1' e2' "tmp" builder
            | _         -> raise (Failure "internal error: semant should have rejected and/or on int")
        )

        (* Binary matrix operations *)
        | A.Matrix(ty, rows, inner) -> (match e2 with
            | (A.Matrix(ty, _, cols), _) ->
                let copy1 = L.build_alloca (ltype_of_typ t) "copy" builder in 
                let _ = L.build_store e1' copy1 builder 
                and copy2 = L.build_alloca (ltype_of_typ (fst e2)) "copy" builder in 
                let _ = L.build_store e2' copy2 builder 
                and result = L.build_alloca (array_t (array_t (ltype_of_typ ty) cols) rows) "result" builder in (match ty with 
                    | A.Bool -> 
                        for i = 0 to rows - 1 do
                            for j = 0 to cols - 1 do
                                let v1 = L.build_load (L.build_gep copy1 [| zero; lint i; lint j |] "gep" builder) "load" builder 
                                and v2 = L.build_load (L.build_gep copy2 [| zero; lint i; lint j |] "gep" builder) "load" builder in
                                let op_res = (bop_of op) v1 v2 "op_res" builder 
                                and reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder
                                in ignore (L.build_store op_res reg builder)
                            done;
                        done; L.build_load result "result" builder
                    | A.Int -> (match op with
                        | A.Mult -> 
                            for i = 0 to rows - 1 do
                                for j = 0 to cols - 1 do
                                    let accum = ref zero in
                                    for k = 0 to inner - 1 do
                                        let v1 = L.build_load (L.build_gep copy1 [| zero; lint i; lint k |] "gep" builder) "load" builder 
                                        and v2 = L.build_load (L.build_gep copy2 [| zero; lint k; lint j |] "gep" builder) "load" builder in
                                        let prod = L.build_mul v1 v2 "prod" builder 
                                        in accum := L.build_add !accum prod "sum" builder
                                    done;
                                    let reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder 
                                    in ignore (L.build_store !accum reg builder)
                                done;
                            done; L.build_load result "prod" builder
                        | _ -> 
                            for i = 0 to rows - 1 do
                                for j = 0 to cols - 1 do
                                    let v1 = L.build_load (L.build_gep copy1 [| zero; lint i; lint j |] "gep" builder) "load" builder 
                                    and v2 = L.build_load (L.build_gep copy2 [| zero; lint i; lint j |] "gep" builder) "load" builder in
                                    let op_res = (iop_of op) v1 v2 "op_res" builder 
                                    and reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder
                                    in ignore (L.build_store op_res reg builder)
                                done;
                            done; L.build_load result "result" builder
                    )
                    | A.Float -> (match op with
                        | A.Mult -> 
                            for i = 0 to rows - 1 do
                                for j = 0 to cols - 1 do
                                    let accum = ref (lfloat 0.) in
                                    for k = 0 to inner - 1 do
                                        let v1 = L.build_load (L.build_gep copy1 [| zero; lint i; lint k |] "gep" builder) "load" builder 
                                        and v2 = L.build_load (L.build_gep copy2 [| zero; lint k; lint j |] "gep" builder) "load" builder in
                                        let prod = L.build_fmul v1 v2 "prod" builder 
                                        in accum := L.build_fadd !accum prod "sum" builder
                                    done;
                                    let reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder 
                                    in ignore (L.build_store !accum reg builder)
                                done;
                            done; L.build_load result "prod" builder
                        | _ -> 
                            for i = 0 to rows - 1 do
                                for j = 0 to cols - 1 do
                                    let v1 = L.build_load (L.build_gep copy1 [| zero; lint i; lint j |] "gep" builder) "load" builder 
                                    and v2 = L.build_load (L.build_gep copy2 [| zero; lint i; lint j |] "gep" builder) "load" builder in
                                    let op_res = (fop_of op) v1 v2 "op_res" builder 
                                    and reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder
                                    in ignore (L.build_store op_res reg builder)
                                done;
                            done; L.build_load result "result" builder
                    )
                    | _ -> raise (Failure "unsupported matrix type")
                )
                | (A.Int, _) when op = A.Exp -> 
                    let copy = L.build_alloca (ltype_of_typ t) "copy" builder in 
                    let _ = L.build_store e1' copy builder 
                    and tmp = L.build_alloca (ltype_of_typ t) "tmp" builder in 
                    let _ = L.build_store e1' tmp builder
                    and result = L.build_alloca (ltype_of_typ t) "result" builder in   

                    for i = 0 to rows - 1 do
                        for j = 0 to rows - 1 do
                            let reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder
                            and v = if i = j then if ty = A.Int then one else lfloat 1.
                                    else if ty = A.Int then zero else lfloat 0.
                            in ignore (L.build_store v reg builder)
                        done;
                    done;

                    let exp = ref e2' in
                    while not (L.is_null !exp) do
                        for i = 0 to rows - 1 do
                            for j = 0 to rows - 1 do
                                let accum = ref (if ty = A.Int then zero else lfloat 0.) in
                                for k = 0 to rows - 1 do
                                    let v1 = L.build_load (L.build_gep tmp [| zero; lint i; lint k |] "gep" builder) "load" builder 
                                    and v2 = L.build_load (L.build_gep copy [| zero; lint k; lint j |] "gep" builder) "load" builder in
                                    let prod = (if ty = A.Int then L.build_mul else L.build_fmul) v1 v2 "prod" builder 
                                    in accum := (if ty = A.Int then L.build_add else L.build_fadd) !accum prod "sum" builder 
                                done;
                                let reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder 
                                in ignore (L.build_store !accum reg builder)
                            done;
                        done; 
                        let _ = L.build_store (L.build_load result "load" builder) tmp builder
                        in exp := L.build_sub !exp one "diff" builder
                    done; L.build_load result "exp" builder
                | (ty, _) when op = A.Mult || op = A.Div -> 
                        let result = L.build_alloca (ltype_of_typ t) "result" builder in
                        let _ = L.build_store e1' result builder in

                        let func = function
                            | (A.Int, A.Mult)   -> L.build_mul
                            | (A.Int, A.Div)    -> L.build_sdiv
                            | (A.Float, A.Mult) -> L.build_fmul
                            | (A.Float, A.Div)  -> L.build_fdiv
                            | _ -> raise (Invalid_argument "invalid (type, operator) pair") 
                        in

                        for i = 0 to rows - 1 do
                            for j = 0 to inner - 1 do
                                let reg = L.build_gep result [| zero; lint i; lint j |] "gep" builder in
                                let v = L.build_load reg "load" builder in
                                let entry = (func (ty, op)) v e2' "entry" builder
                                in ignore (L.build_store entry reg builder)
                            done;
                        done; L.build_load result "result" builder    
                | _ -> raise (Invalid_argument "invalid arguments to matrix binary operator")
            )
            | _ -> raise (Invalid_argument "invalid argument type")
        )
    | SUnop(op, e) ->
  	    let (t, s) = e and e' = expr builder e in
        (* Check for Variable identifier *)
        let is_var v = (match v with
            SId(_) -> true
          | _      -> false
        )
        in
        (* Get variable identifier string *)
        let get_var_str v = (match v with
            SId(i) -> i
          | _      -> "whomp"
        )
        in
        let sid = if is_var s then get_var_str s else "whomp" in
        (match op with
          A.Neg when t = A.Float -> L.build_fneg e' "tmp" builder
  	    | A.Neg                  -> L.build_neg e' "tmp" builder
        | A.Not                  -> L.build_not e' "tmp" builder
        (* Increment++/Decrement-- only work if operand is a variable identifier *)
        | A.Inc when t = A.Int && is_var s  -> let new_val = L.build_add e' (L.const_int i32_t 1) "tmp" builder
                                    in let _ = L.build_store new_val (lookup sid) builder
                                    in L.build_load (lookup sid) "tmp" builder
        | A.Dec when t = A.Int && is_var s  -> let new_val = L.build_sub e' (L.const_int i32_t 1) "tmp" builder
                                    in let _ = L.build_store new_val (lookup sid) builder
                                    in L.build_load (lookup sid) "tmp" builder
        | A.Trans -> (match t with 
            | A.Matrix(ty, rows, cols) -> 
                let copy = L.build_alloca (ltype_of_typ t) "copy" builder in
                let _ = L.build_store e' copy builder
                and result = L.build_alloca (array_t (array_t (ltype_of_typ ty) rows) cols) "result" builder in 

                for i = 0 to rows - 1 do
                    for j = 0 to cols - 1 do
                        let v = L.build_load (L.build_gep copy [| zero; lint i; lint j |] "gep" builder) "load" builder 
                        and reg = L.build_gep result [| zero; lint j; lint i |] "gep" builder 
                        in ignore (L.build_store v reg builder)
                    done;
                done; L.build_load result "trans" builder
            | _ -> raise (Failure "internal error: operator not allowed")
            )
        | _ -> raise (Failure "internal error: operator not allowed")
        )
    | SAssign (s, e) -> let e' = expr builder e in let _  = L.build_store e' (lookup s) builder in e'
    | SCall ("print", [e]) ->
        let e' = expr builder e in
        (match (type_of_lvalue e') with
          A.Int    -> L.build_call printf_func [| int_format_str ; (e') |] "print" builder
        | A.Float  -> L.build_call printf_func [| float_format_str ; (e') |] "printflt" builder
        | A.String -> L.build_call printf_func [| string_format_str ; (e') |] "printstr" builder
        | A.Bool   -> L.build_call printf_func [| int_format_str ; (e') |] "printb" builder
        | _        -> raise (Failure "invalid print operation")
        )
    | SCall ("println", [e]) ->
        let e' = expr builder e in
        (match (type_of_lvalue e') with
          A.Int    -> L.build_call printf_func [| int_format_str_n ; (e') |] "print" builder
        | A.Float  -> L.build_call printf_func [| float_format_str_n ; (e') |] "printflt" builder
        | A.String -> L.build_call printf_func [| string_format_str_n ; (e') |] "printstr" builder
        | A.Bool   -> L.build_call printf_func [| int_format_str_n ; (e') |] "printb" builder
        | _        -> raise (Failure "invalid print operation")
        )
    | SCall ("rows", [e]) -> (match e with
        | (A.Matrix(_, rows, _), _) -> lint rows
        | _ -> raise (Invalid_argument "illegal argument to rows")
        ) 
    | SCall ("cols", [e]) -> (match e with
        | (A.Matrix(_, _, cols), _) -> lint cols
        | _ -> raise (Invalid_argument "illegal argument to cols")
        ) 
    (* casting *)
    | SCall ("ftoi", [e]) -> 
        L.build_fptosi (expr builder e) i32_t "fto" builder
    | SCall ("itof", [e]) ->
        L.build_sitofp (expr builder e) float_t "ito" builder
    (* | SCall ("printstr", [e]) -> L.build_call printf_func [| string_format_str ; (expr builder e) |] "printstr" builder *)
    | SMatLit(mat, _, _) -> 
        let lty = ltype_of_typ (fst (List.hd (List.hd mat))) in 
        let expr_lists = List.map (List.map (expr builder)) mat  in
        let list_of_arrays = List.map Array.of_list expr_lists in
        let list_of_larrays = List.map (L.const_array lty) list_of_arrays in
        let array_of_larrays = Array.of_list list_of_larrays in
        L.const_array (array_t lty (List.length (List.hd mat))) array_of_larrays
    | SMatAccess (id, row, col) ->
        let row = expr builder row in
        let col = expr builder col in
        let reg = L.build_gep (lookup id) [| zero; row; col |] id builder in
        L.build_load reg id builder
    | SMatAssign (id, row, col, value) ->
        let row   = expr builder row in
        let col   = expr builder col in
        let reg   = L.build_gep (lookup id) [| L.const_int i32_t 0; row; col |] id builder in
        let value = expr builder value in
        L.build_store value reg builder
  (* | SCall ("size", [e]) ->
      L.build_call size_func [| (expr builder e) |]
        "size" builder
  | SCall ("det", [e]) ->
          L.build_call det_func [| (expr builder e) |]
        "det" builder
  | SCall ("minor", [e]) ->
          L.build_call minor_func [| (expr builder e) |]
        "minor" builder
  | SCall ("inv", [e]) ->
          L.build_call inv_func [| (expr builder e) |]
        "inv" builder
  | SCall ("tr", [e]) ->
          L.build_call tr_func [| (expr builder e) |]
        "tr" builder *)
    (* | SCall ("printflt", [e]) -> L.build_call printf_func [| float_format_str ; (expr builder e) |] "printflt" builder *)
    | SCall (f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in
        let result = (match fdecl.styp with
            | A.Void -> ""
            | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
and produce control flow, not values *)
    (* Invoke "f builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder f =
        (* The current block where we're inserting instr *)
        match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
  | None -> ignore (f builder) in

    (* Create a dummy blockent for creating SBlocks in codegen. We already
     * checked declarations and such in semant so what we put in the SBlock
     * doesn't really matter now. *)
    let db = {
        sparent = None;
            symtbl = Hashtbl.create 1;
    } in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
        SBlock (sl, _) -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
  | SVDecl (t, n, e) ->
          let _ = add_local local_vars (t, n) builder in
          let _ = if (snd e) != SNoexpr
                                    then (expr builder (t, SAssign(n, e)))
                                    else (expr builder (t, SNoexpr))
          in builder
  | SExpr e -> let _ = expr builder e in builder

      | SContinue -> let _ = L.build_br (List.hd !continue_stack) builder in builder
      | SBreak n -> let _ = L.build_br (List.nth !break_stack (n - 1)) builder in builder
      | SReturn e -> let _ = match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder
                              (* Build return statement *)
      | _ -> L.build_ret (expr builder e) builder
      in builder
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
          they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | SIf (predicate, then_stmt, else_stmt) ->
              let bool_val = expr builder predicate in
              (* Add "merge" basic block to our function's list of blocks *)
              let merge_bb = L.append_block context "merge" the_function in
              (* Partial function used to generate branch to merge block *)
              let branch_instr = L.build_br merge_bb in

              (* Same for "then" basic block *)
              let then_bb = L.append_block context "then" the_function in
              (* Position builder in "then" block and build the statement *)
    let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
    (* Add a branch to the "then" block (to the merge block)
           if a terminator doesn't already exist for the "then" block *)
    let () = add_terminal then_builder branch_instr in

    (* Identical to stuff we did for "then" *)
    let else_bb = L.append_block context "else" the_function in
    let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
    let () = add_terminal else_builder branch_instr in

    (* Generate initial branch instruction perform the selection of "then"
         or "else". Note we're using the builder we had access to at the start
         of this alternative. *)
    let _ = L.build_cond_br bool_val then_bb else_bb builder in
    (* Move to the merge block for further instruction building *)
    L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
              (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
    let pred_bb = L.append_block context "while" the_function in
    (* In current block, branch to predicate to execute the condition *)
    let _ = L.build_br pred_bb builder in

    (* Make room for a body block for now. We'll build it later. *)
    let body_bb = L.append_block context "while_body" the_function in

    (* Generate the predicate code in the predicate block *)
    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    (* Hook everything up *)
    let merge_bb = L.append_block context "merge" the_function in
    let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
    let _ = L.builder_at_end context merge_bb in

    (* Add the predicate and merge blocks to our lists of jump points *)
    let _ = continue_stack := pred_bb :: !continue_stack in
    let _ = break_stack := merge_bb :: !break_stack in

    (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
    let while_builder = stmt (L.builder_at_end context body_bb) body in

    (* Now that we've exited a loop level, pop the jump stacks *)
    let _ = continue_stack := List.tl !continue_stack in
    let _ = break_stack := List.tl !break_stack in

    let () = add_terminal while_builder (L.build_br pred_bb)
    in L.builder_at_end context merge_bb

      (* To support `continue` in for loops, we need to reimplement SWhile with
       * the post-loop action added to the merge basic block *)
      | SFor (preact, predicate, postact, body) ->
              (* Emit preact first before we continue *)
              let _ = expr builder preact in

              let pred_bb = L.append_block context "while" the_function in
              let _ = L.build_br pred_bb builder in

              let body_bb = L.append_block context "while_body" the_function in

              let pred_builder = L.builder_at_end context pred_bb in
              let bool_val = expr pred_builder predicate in

              let merge_bb = L.append_block context "merge" the_function in
              let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
              let _ = L.builder_at_end context merge_bb in

              (* Emit a separate block for the post-action that continue statements
               * can branch to *)
    let postact_bb = L.append_block context "postact" the_function in

    (* Add the post-action and merge blocks to our lists of jump points *)
    let _ = continue_stack := postact_bb :: !continue_stack in
    let _ = break_stack := merge_bb :: !break_stack in

    let while_builder = stmt (L.builder_at_end context body_bb) body in

    (* Pop the jump stacks *)
    let _ = continue_stack := List.tl !continue_stack in
    let _ = break_stack := List.tl !break_stack in

    (* Emit the post-action itself *)
    let postact_builder = L.builder_at_end context postact_bb in
    let _ = expr postact_builder postact in

    let _ = add_terminal postact_builder (L.build_br pred_bb) in
    let () = add_terminal while_builder (L.build_br postact_bb)
    in L.builder_at_end context merge_bb
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock (fdecl.sbody, db)) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in

  List.iter build_function_body functions;
  the_module

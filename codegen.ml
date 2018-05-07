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

module B = Batteries
module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
    let context = L.global_context () in
    (* Create an LLVM module -- this is a "container" into which we'll
       generate actual code *)
    let the_module = L.create_module context "autoMATic" in
    (* Add types to the context so we can use them in our LLVM code *)
    let i32_t     = L.i32_type          context
    and i8_t      = L.i8_type           context
    and i1_t      = L.i1_type           context
    and float_t   = L.double_type       context
    and void_t    = L.void_type         context
    and pointer_t = L.pointer_type      in
    let str_t     = L.pointer_type i8_t
    and array_t   = L.array_type
    and zero      = L.const_int i32_t 0
    and one       = L.const_int i32_t 1 in
    (* Create named structs for each of the matrix types, name encodes the type of the matrix
       Struct only contains a pointer to some value.
       This needs to be casted to the appropriate matrix [n x [m x ty]] type before performing operations*)
    let matrix_i   = L.named_struct_type context "matrix_i" in
                     (* L.struct_set_body matrix_i [|i32_t; i32_t; i32_t; i8_t; L.pointer_type i32_t|] false; *)
                     L.struct_set_body matrix_i [| L.pointer_type i32_t; i32_t; i32_t |] false;
    let matrix_f   = L.named_struct_type context "matrix_f" in
                     L.struct_set_body matrix_f [| L.pointer_type float_t; i32_t; i32_t |] false;
    let matrix_b   = L.named_struct_type context "matrix_b" in
                     L.struct_set_body matrix_b [| L.pointer_type i1_t; i32_t; i32_t |] false;

(* === Helpers for autoMATic/LLVM types === *)
(* Convert autoMATic types to LLVM types *)
let ltype_of_typ = function
      A.Int    -> i32_t
    | A.Bool   -> i1_t
    | A.Float  -> float_t
    | A.Void   -> void_t
    | A.String -> str_t
    (* | A.Matrix(typ, rows, cols) -> (match typ with
                                        | A.Int   -> array_t (array_t i32_t cols)   rows
                                        | A.Bool  -> array_t (array_t i1_t cols)    rows
                                        | A.Float -> array_t (array_t float_t cols) rows
                                        | _       -> raise (Failure "internal error: invalid matrix type")) *)
    | A.Matrix(typ, _, _) -> (match typ with
                                          A.Int   -> matrix_i
                                        | A.Float -> matrix_f
                                        | A.Bool  -> matrix_b
                                        | _       -> raise(Failure "invalid matrix type")
                                        )
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
  | "%matrix_i*" -> A.Matrix(A.Int, 0,0)
  | "%matrix_b*" -> A.Matrix(A.Bool, 0,0)
  | "%matrix_f*" -> A.Matrix(A.Float, 0,0)
  | _       -> raise (Failure "invalid type"))
in

let type_of_lvalue lv =
  let lltype = L.type_of lv in
  type_of_llvalue lltype
in

(* Checks if pointer is to a matrix *)
let is_matrix_ptr ptr =
  let ltype_string = L.string_of_lltype (L.type_of ptr) in
  (match ltype_string with
     "%matrix_i*" | "%matrix_f*" | "%matrix_b*" -> true
   | _  -> false
   )
in

(* Checks if lltype is a matrix *)
let is_matrix ptr =
  let ltype_string = L.string_of_lltype (L.type_of ptr) in
  (match ltype_string with
     "%matrix_i" | "%matrix_f" | "%matrix_b" -> true
   | _  -> false
   )
in

(* Initialization helper: function used to initialize global and local variables *)
let empty_string = L.define_global "__empty_string" (L.const_stringz context "") the_module in
let init_var typ = (match typ with
                      A.Int   -> L.const_int i32_t 0
                    | A.Float -> L.const_float float_t 0.0
                    | A.Bool -> L.const_int i1_t 0
                    | A.String -> L.const_bitcast empty_string str_t
                    | A.Void -> L.const_null void_t
                    | A.Matrix(ty, _, _) -> (match ty with
                                              A.Int   -> L.const_null matrix_i
                                            | A.Float -> L.const_null matrix_f
                                            | A.Bool  -> L.const_null matrix_b
                                            | _       -> raise (Failure "error: invalid matrix type"))
                    | _ -> L.const_int i32_t 0
                    )
in

(* Declare each global variable; remember its value in a map *)
  let global_vars = Hashtbl.create 100 in
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0 in
      Hashtbl.replace m n ((L.define_global n init the_module), t);
    in
    let add_to_globals x = global_var global_vars x in
    let _ = List.iter add_to_globals globals
  in

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
let function_decls = Hashtbl.create 100 in
    let function_decl m fdecl =
        let name = fdecl.sfname
        (* and formal_list = (List.map (fun (t, _) -> (ltype_of_typ t)) fdecl.sformals) *)
        and formal_types = Array.of_list (List.map (fun (t,_) -> (ltype_of_typ t)) fdecl.sformals) in
        let ftype = L.function_type ((ltype_of_typ fdecl.styp)) formal_types
        in
        let _ = print_string ("\nfunction type " ^ L.string_of_lltype ftype) in
        Hashtbl.add m name (L.define_function name ftype the_module, fdecl)
    in
    let add_to_function_decls x = function_decl function_decls x in
    let _ = List.iter add_to_function_decls functions
    in

        (* StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty functions in *)

let lookup_func f function_decls = try ((Hashtbl.find function_decls f))
                    with Not_found -> raise (Failure "function not found")
in

(* Fill in the body of the given function *)
let build_function_body fdecl =
    let (the_function, _) = lookup_func fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%f\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Keep track of jump locations for breaking out of/continuing loops *)
    let continue_stack = ref []
    and break_stack = ref []
    in

    (* Allocate space for any locally declared variables and add the
     * resulting registers to our map *)
    let add_local m (t, n) builder =
      let local_var = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store (init_var t) local_var builder);
      Hashtbl.add m n (local_var, t)
    in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars = Hashtbl.create 100 in
      let add_formal m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        Hashtbl.add m n (local, t)
      in

      let add_formal_to_locals x y = add_formal local_vars x y in
      let _ = List.iter2 add_formal_to_locals fdecl.sformals (Array.to_list (L.params the_function))
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try fst (Hashtbl.find local_vars n)
                   with Not_found -> fst (Hashtbl.find global_vars n)
    in

    let lookup_map n = try (fst (Hashtbl.find local_vars n), local_vars)
                       with Not_found -> (fst (Hashtbl.find global_vars n), global_vars)
    in

    let lookup_typ n = try snd (Hashtbl.find local_vars n)
                       with Not_found -> snd (Hashtbl.find global_vars n)
    in

    (* === Matrix Builder Helpers === *)
    let build_arr_from_list init_mat lltype expr builder =
      let lltype_lists = List.map (List.map (expr builder)) init_mat in
      let list_of_arrays = List.map Array.of_list lltype_lists in
      let lltype_list_of_arrays = List.map (L.const_array lltype) list_of_arrays in
      let array_of_arrays = Array.of_list lltype_list_of_arrays in
      L.const_array (array_t lltype (List.length (List.hd init_mat))) array_of_arrays
    in

    let build_mat_lit mat_lit r c mat_type lltype expr builder =
      let arr_mat = build_arr_from_list (mat_lit) (lltype) (expr) (builder) in
      let ll_mat = L.build_alloca (array_t (array_t lltype c) r) "lit_mat" builder in
      let _ = ignore (L.build_store arr_mat ll_mat builder) in
      let m = L.build_alloca mat_type "m" builder in
      let struct_mat = L.build_struct_gep m 0 "mat_struct" builder in
      let struct_mat_cast = L.build_bitcast struct_mat (pointer_t (pointer_t (array_t (array_t lltype c) r))) "m_mat_cast" builder in
        ignore(L.build_store ll_mat struct_mat_cast builder);
      let m_r = L.build_struct_gep m 1 "m_r" builder in
        ignore(L.build_store (L.const_int i32_t r) m_r builder);
      let m_c = L.build_struct_gep m 2 "m_c" builder in
        ignore(L.build_store (L.const_int i32_t c) m_c builder); m
    in

    let build_mat_init prev_mat r c mat_type lltype expr builder =
      let init_mat = if lltype = float_t then B.List.make r (B.List.make c (A.Float, SFloatLit(0.0)))
                     else if lltype = i1_t then B.List.make r (B.List.make c (A.Bool, SBoolLit(false)))
                     else if lltype = i32_t then B.List.make r (B.List.make c (A.Int, SIntLit(0)))
                     else raise (Failure "invalid matrix type")
      in
      let arr_mat = build_arr_from_list (init_mat) (lltype) (expr) (builder) in
      let ll_mat = L.build_alloca (array_t (array_t lltype c) r) "init_mat" builder in
      let _ = ignore (L.build_store arr_mat ll_mat builder) in
      let m = prev_mat in
      let struct_mat = L.build_struct_gep m 0 "mat_struct" builder in
      let struct_mat_cast = L.build_bitcast struct_mat (pointer_t (pointer_t (array_t (array_t lltype c) r))) "m_mat_cast" builder in
        ignore(L.build_store ll_mat struct_mat_cast builder);
      let m_r = L.build_struct_gep m 1 "m_r" builder in
        ignore(L.build_store (L.const_int i32_t r) m_r builder);
      let m_c = L.build_struct_gep m 2 "m_c" builder in
        ignore(L.build_store (L.const_int i32_t c) m_c builder); m
    in

    (* Helper function to reassign matrices *)
    let reassign_mat old_mat new_mat r c lltype builder =
      let old_cast = L.build_bitcast (L.build_struct_gep old_mat 0 "old_mat" builder)  (pointer_t (pointer_t (array_t (array_t lltype c) r))) "old_mat_cast" builder in
      let new_cast = L.build_bitcast (L.build_struct_gep new_mat 0 "new_mat" builder)  (pointer_t (pointer_t (array_t (array_t lltype c) r))) "new_mat_cast" builder in
      let new_data = L.build_load (new_cast) "new_data" builder in
      ignore (L.build_store new_data old_cast builder)
    in

    (* Extract typ from a (typ * sexpr) tuple *)
    let get_styp e = fst e
    in

    let rec expr builder (_, e) = match e with
    | SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SFloatLit l -> L.const_float float_t l
    | SStrLit s -> L.build_global_stringptr s "" builder
    | SNoexpr -> L.const_int i32_t 0
    | SId s -> let ptr = lookup s in
      (match (is_matrix_ptr ptr) with
        true  -> ptr
      | false -> L.build_load (lookup s) s builder
      )
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
            | A.Mult    -> L.build_fmul e1' e2' "tmp" builder
            | A.Exp     -> let (ty, _) = e2 in
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
            | _         -> raise (Failure "internal error: semant should have rejected and/or on float"))
        (* Binary integer operations *)
        | A.Int -> (match op with
            | A.Add     -> L.build_add e1' e2' "tmp" builder
            | A.Sub     -> L.build_sub e1' e2' "tmp" builder
            | A.Mult    -> L.build_mul e1' e2' "tmp" builder
            | A.Exp     -> let (ty, _) = e2 in
                           let cast = L.build_sitofp e1' float_t "cast" builder
                           and safe_cast = if ty = A.Float then e2' else L.build_sitofp e2' float_t "safe_cast" builder in
                           let result = L.build_call pow_func [| cast; safe_cast |] "exp" builder in
                           let return = if ty = A.Int then L.build_fptosi result i32_t "result" builder else result
                           in return
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
            | _         -> raise (Failure "internal error: semant should have rejected and/or on int"))
        (* Binary matrix operations *)
        | A.Matrix(ty, rows, mid1) -> (match e2 with
            | (A.Matrix(ty, mid2, cols), _) -> (match ty with
                (* Binary boolean matrix operations *)
                | A.Bool -> let copy1 = L.build_alloca (array_t (array_t i1_t mid1) rows) "copy" builder in
                            let e_m1 = L.build_bitcast (L.build_struct_gep e1' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i1_t mid1) rows))) "copy_res" builder in
                            let _ = L.build_store (L.build_load (L.build_load e_m1 "load_ptr" builder) "load_mat" builder) copy1 builder in
                            (* let _ = L.build_store e1' copy1 builder in *)
                            let copy2 = L.build_alloca (array_t (array_t i1_t cols) mid2) "copy" builder in
                            let e_m2 = L.build_bitcast (L.build_struct_gep e2' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i1_t cols) mid2))) "copy_res" builder in
                            let _ = L.build_store (L.build_load (L.build_load e_m2 "load_ptr" builder) "load_mat" builder) copy2 builder

                            and result = L.build_alloca (array_t (array_t i1_t cols) rows) "result" builder in
                            let m = (L.build_alloca matrix_b "b_init" builder) in
                            let result_struct = build_mat_init m rows cols matrix_b i1_t expr builder in
                            let result_cast = L.build_bitcast (L.build_struct_gep m 0 "b_cast" builder)  (pointer_t (pointer_t (array_t (array_t i1_t cols) rows))) "b_result" builder in

                    (match op with
                    (*| A.Add      -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let sum = L.build_add v1 v2 "sum" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store sum reg builder)
                                        done;
                                    done;
                                    L.build_load result "sum" builder

                    | A.Sub      -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let diff = L.build_sub v1 v2 "diff" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store diff reg builder)
                                        done;
                                    done; L.build_load result "diff" builder*)

                    | A.And      -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let and_res = L.build_and v1 v2 "and" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store and_res reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.Or       -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let or_res = L.build_or v1 v2 "or" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store or_res reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | _ -> raise (Invalid_argument "invalid matrix binary operator"))
                | A.Int -> let copy1 = L.build_alloca (array_t (array_t i32_t mid1) rows) "copy" builder in
                           let e_m1 = L.build_bitcast (L.build_struct_gep e1' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t mid1) rows))) "copy_res" builder in
                           let _ = L.build_store (L.build_load (L.build_load e_m1 "load_ptr" builder) "load_mat" builder) copy1 builder in
                           let copy2 = L.build_alloca (array_t (array_t i32_t cols) mid2) "copy" builder in
                           let e_m2 = L.build_bitcast (L.build_struct_gep e2' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t cols) mid2))) "copy_res" builder in
                           let _ = L.build_store (L.build_load (L.build_load e_m2 "load_ptr" builder) "load_mat" builder) copy2 builder

                           and result = L.build_alloca (array_t (array_t i32_t cols) rows) "result" builder in
                           let m = L.build_alloca matrix_i "i_init" builder in
                           let result_struct = build_mat_init m rows cols matrix_i i32_t expr builder in
                           let result_cast = L.build_bitcast (L.build_struct_gep m 0 "i_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t cols) rows))) "i_result" builder in

                    (match op with
                    | A.Add      -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let sum = L.build_add v1 v2 "sum" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store sum reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.Sub      -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let diff = L.build_sub v1 v2 "diff" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store diff reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.Mult     -> for i = 0 to rows - 1 do
                                        for j = 0 to cols - 1 do
                                            let reg = L.build_gep result [| zero; L.const_int i32_t i; L.const_int i32_t j |] "gep" builder
                                            in ignore (L.build_store zero reg builder)
                                        done;
                                    done;

                                    for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            for k = 0 to mid1 - 1 do
                                                let m = L.const_int i32_t k in
                                                let v1 = L.build_load (L.build_gep copy1
                                                    [| zero; row; m |] "gep" builder) "load" builder
                                                and v2 = L.build_load (L.build_gep copy2
                                                    [| zero; m; col |] "gep" builder) "load" builder in
                                                let prod = L.build_mul v1 v2 "prod" builder
                                                and reg = L.build_gep result [| zero; row; col |] "gep" builder in
                                                let accum = L.build_load reg "accum" builder in
                                                let sum = L.build_add accum prod "sum" builder
                                                in ignore (L.build_store sum reg builder)
                                            done;
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.ElemMult -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let prod = L.build_mul v1 v2 "prod" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store prod reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.ElemDiv  -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let quot = L.build_sdiv v1 v2 "quot" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store quot reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | _ -> raise (Invalid_argument "invalid matrix binary operator"))
                | A.Float -> let copy1 = L.build_alloca (array_t (array_t float_t mid1) rows) "copy" builder in
                             let e_m1 = L.build_bitcast (L.build_struct_gep e1' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t mid1) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_m1 "load_ptr" builder) "load_mat" builder) copy1 builder in
                             let copy2 = L.build_alloca (array_t (array_t float_t cols) mid2) "copy" builder in
                             let e_m2 = L.build_bitcast (L.build_struct_gep e2' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t cols) mid2))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_m2 "load_ptr" builder) "load_mat" builder) copy2 builder

                             and result = L.build_alloca (array_t (array_t float_t cols) rows) "result" builder in
                             let m = L.build_alloca matrix_f "f_init" builder in
                             let result_struct = build_mat_init m rows cols matrix_f float_t expr builder in
                             let result_cast = L.build_bitcast (L.build_struct_gep m 0 "f_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t cols) rows))) "f_result" builder in

                    (match op with
                    | A.Add      -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let sum = L.build_fadd v1 v2 "sum" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store sum reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.Sub      -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let diff = L.build_fsub v1 v2 "diff" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store diff reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.Mult     -> for i = 0 to rows - 1 do
                                        for j = 0 to cols - 1 do
                                            let reg = L.build_gep result [| zero; L.const_int i32_t i; L.const_int i32_t j |] "gep" builder
                                            in ignore (L.build_store (L.const_float float_t 0.) reg builder)
                                        done;
                                    done;

                                    for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            for k = 0 to mid1 - 1 do
                                                let m = L.const_int i32_t k in
                                                let v1 = L.build_load (L.build_gep copy1
                                                    [| zero; row; m |] "gep" builder) "load" builder
                                                and v2 = L.build_load (L.build_gep copy2
                                                    [| zero; m; col |] "gep" builder) "load" builder in
                                                let prod = L.build_fmul v1 v2 "prod" builder
                                                and reg = L.build_gep result [| zero; row; col |] "gep" builder in
                                                let accum = L.build_load reg "accum" builder in
                                                let sum = L.build_fadd accum prod "sum" builder
                                                in ignore (L.build_store sum reg builder)
                                            done;
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.ElemMult -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let prod = L.build_fmul v1 v2 "prod" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store prod reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | A.ElemDiv  -> for i = 0 to rows - 1 do
                                        let row = L.const_int i32_t i in
                                        for j = 0 to cols - 1 do
                                            let col = L.const_int i32_t j in
                                            let v1 = L.build_load (L.build_gep copy1
                                                [| zero; row; col |] "gep" builder) "load" builder
                                            and v2 = L.build_load (L.build_gep copy2
                                                [| zero; row; col |] "gep" builder) "load" builder in
                                            let quot = L.build_fdiv v1 v2 "quot" builder
                                            and reg = L.build_gep result [| zero; row; col |] "gep" builder
                                            in ignore (L.build_store quot reg builder)
                                        done;
                                    done; ignore (L.build_store result result_cast builder); result_struct
                    | _ -> raise (Invalid_argument "invalid matrix binary operator"))
                | _ -> raise (Failure "unsupported matrix type"))
            | (A.Int, SIntLit n) -> (match ty with
                | A.Int   -> let copy = L.build_alloca (array_t (array_t i32_t rows) rows) "copy" builder in
                             let e_m = L.build_bitcast (L.build_struct_gep e1' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t rows) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_m "load_ptr" builder) "load_mat" builder) copy builder in

                             let tmp = L.build_alloca (array_t (array_t i32_t rows) rows) "tmp" builder in
                             let e_tmp = L.build_bitcast (L.build_struct_gep e1' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t rows) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_tmp "load_ptr" builder) "load_mat" builder) tmp builder

                             and result = L.build_alloca (array_t (array_t i32_t rows) rows) "result" builder in
                             let m = L.build_alloca matrix_i "i_init" builder in
                             let result_struct = build_mat_init m rows rows matrix_i i32_t expr builder in
                             let result_cast = L.build_bitcast (L.build_struct_gep m 0 "i_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t rows) rows))) "i_result" builder in

                             for i = 0 to rows - 1 do
                                 for j = 0 to rows - 1 do
                                     let reg = L.build_gep result [| zero; L.const_int i32_t i; L.const_int i32_t j |] "gep" builder
                                     and v = if i = j then one else zero
                                     in ignore (L.build_store v reg builder)
                                 done;
                             done;

                             for l = 0 to n - 1 do
                                 for i = 0 to rows - 1 do
                                     let row = L.const_int i32_t i in
                                     for j = 0 to rows - 1 do
                                         let col = L.const_int i32_t j and accum = ref zero in
                                         for k = 0 to mid1 - 1 do
                                             let m = L.const_int i32_t k in
                                             let v1 = L.build_load (L.build_gep tmp
                                                 [| zero; row; m |] "gep" builder) "load" builder
                                             and v2 = L.build_load (L.build_gep copy
                                                 [| zero; m; col |] "gep" builder) "load" builder in
                                             let prod = L.build_mul v1 v2 "prod" builder
                                             in accum := L.build_add !accum prod "sum" builder
                                         done;
                                         let reg = L.build_gep result [| zero; row; col |] "gep" builder in
                                         ignore (L.build_store !accum reg builder)
                                     done;
                                 done;
                                 ignore (L.build_store (L.build_load result "load" builder) tmp builder)
                             done; ignore (L.build_store result result_cast builder); result_struct
                | A.Float -> let copy = L.build_alloca (array_t (array_t float_t rows) rows) "copy" builder in
                             let e_m = L.build_bitcast (L.build_struct_gep e1' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t rows) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_m "load_ptr" builder) "load_mat" builder) copy builder in
                             let tmp = L.build_alloca (array_t (array_t float_t rows) rows) "tmp" builder in
                             let e_tmp = L.build_bitcast (L.build_struct_gep e1' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t rows) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_tmp "load_ptr" builder) "load_mat" builder) tmp builder

                             and result = L.build_alloca (array_t (array_t float_t rows) rows) "result" builder in
                             let m = L.build_alloca matrix_f "f_init" builder in
                             let result_struct = build_mat_init m rows rows matrix_f float_t expr builder in
                             let result_cast = L.build_bitcast (L.build_struct_gep m 0 "f_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t rows) rows))) "f_result" builder in

                             for i = 0 to rows - 1 do
                                 for j = 0 to rows - 1 do
                                     let reg = L.build_gep result [| zero; L.const_int i32_t i; L.const_int i32_t j |] "gep" builder
                                     and v = if i = j then (L.const_float float_t 1.) else (L.const_float float_t 0.)
                                     in ignore (L.build_store v reg builder)
                                 done;
                             done;

                             for l = 0 to n - 1 do
                                 for i = 0 to rows - 1 do
                                     let row = L.const_int i32_t i in
                                     for j = 0 to rows - 1 do
                                         let col = L.const_int i32_t j and accum = ref (L.const_float float_t 0.) in
                                         for k = 0 to mid1 - 1 do
                                             let m = L.const_int i32_t k in
                                             let v1 = L.build_load (L.build_gep tmp
                                                 [| zero; row; m |] "gep" builder) "load" builder
                                             and v2 = L.build_load (L.build_gep copy
                                                 [| zero; m; col |] "gep" builder) "load" builder in
                                             let prod = L.build_fmul v1 v2 "prod" builder
                                             in accum := L.build_fadd !accum prod "sum" builder
                                         done;
                                         let reg = L.build_gep result [| zero; row; col |] "gep" builder in
                                         ignore (L.build_store !accum reg builder)
                                     done;
                                 done;
                                 ignore (L.build_store (L.build_load result "load" builder) tmp builder)
                             done; ignore (L.build_store result result_cast builder); result_struct
                | _ -> raise (Failure "unsupported matrix type"))
            | _ -> raise (Invalid_argument "invalid arguments to matrix binary operator"))
        | _ -> raise (Invalid_argument "invalid argument type"))
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
            | A.Matrix(ty, rows, cols) -> (match ty with
                | A.Bool  -> let copy = L.build_alloca (array_t (array_t i1_t cols) rows) "copy" builder in
                             let e_m = L.build_bitcast (L.build_struct_gep e' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i1_t cols) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_m "load_ptr" builder) "load_mat" builder) copy builder
                             and result = L.build_alloca (array_t (array_t i1_t rows) cols) "result" builder in

                             for i = 0 to rows - 1 do
                                 let row = L.const_int i32_t i in
                                 for j = 0 to cols - 1 do
                                     let col = L.const_int i32_t j in
                                     let v = L.build_load (L.build_gep copy [| zero; row; col |] "gep" builder) "load" builder
                                     and reg = L.build_gep result [| zero; col; row |] "gep" builder in
                                     ignore (L.build_store v reg builder)
                                 done;
                             done;

                             let m = (L.build_alloca matrix_b "trans_b_init" builder) in
                             let result_struct = build_mat_init m cols rows matrix_b i1_t expr builder in
                             let result_cast = L.build_bitcast (L.build_struct_gep m 0 "trans_b_cast" builder)  (pointer_t (pointer_t (array_t (array_t i1_t rows) cols))) "trans_b_result" builder in
                             ignore (L.build_store result result_cast builder); result_struct
                | A.Int   -> let copy = L.build_alloca (array_t (array_t i32_t cols) rows) "copy" builder in
                             let e_m = L.build_bitcast (L.build_struct_gep e' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t cols) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_m "load_ptr" builder) "load_mat" builder) copy builder
                             and result = L.build_alloca (array_t (array_t i32_t rows) cols) "result" builder in

                             for i = 0 to rows - 1 do
                                 let row = L.const_int i32_t i in
                                 for j = 0 to cols - 1 do
                                     let col = L.const_int i32_t j in
                                     let v = L.build_load (L.build_gep copy [| zero; row; col |] "gep" builder) "load" builder
                                     and reg = L.build_gep result [| zero; col; row |] "gep" builder in
                                     ignore (L.build_store v reg builder)
                                 done;
                             done;
                             (* L.build_load result "trans" builder *)
                             let m = L.build_alloca matrix_i "trans_i_init" builder in
                             let result_struct = build_mat_init m cols rows matrix_i i32_t expr builder in
                             let result_cast = L.build_bitcast (L.build_struct_gep m 0 "trans_i_cast" builder)  (pointer_t (pointer_t (array_t (array_t i32_t rows) cols))) "trans_i_result" builder in
                             ignore (L.build_store result result_cast builder); result_struct
                | A.Float -> let copy = L.build_alloca (array_t (array_t float_t cols) rows) "copy" builder in
                             let e_m = L.build_bitcast (L.build_struct_gep e' 0 "copy_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t cols) rows))) "copy_res" builder in
                             let _ = L.build_store (L.build_load (L.build_load e_m "load_ptr" builder) "load_mat" builder) copy builder
                             and result = L.build_alloca (array_t (array_t float_t rows) cols) "result" builder in

                             for i = 0 to rows - 1 do
                                 let row = L.const_int i32_t i in
                                 for j = 0 to cols - 1 do
                                     let col = L.const_int i32_t j in
                                     let v = L.build_load (L.build_gep copy [| zero; row; col |] "gep" builder) "load" builder
                                     and reg = L.build_gep result [| zero; col; row |] "gep" builder in
                                     ignore (L.build_store v reg builder)
                                 done;
                             done;

                             let m = L.build_alloca matrix_f "trans_f_init" builder in
                             let result_struct = build_mat_init m cols rows matrix_f float_t expr builder in
                             let result_cast = L.build_bitcast (L.build_struct_gep m 0 "trans_f_cast" builder)  (pointer_t (pointer_t (array_t (array_t float_t rows) cols))) "trans_f_result" builder in
                             ignore (L.build_store result result_cast builder); result_struct

                | _ -> raise (Failure "internal error: operator not allowed"))
            | _ -> raise (Failure "internal error: operator not allowed"))
        | _ -> raise (Failure "internal error: operator not allowed"))
    | SAssign (s, e) ->
        let stack_build_mat_init prev_mat r c mat_type lltype expr builder =
          build_mat_init prev_mat r c mat_type lltype expr builder
        in
        let e' = expr builder e in
        let rh_ty = get_styp e in
        (* Get llvalue of identifier s *)
        let (ptr, mp) = lookup_map s in
        (* Check if both LHS and RHS are matrices *)
        (match (is_matrix_ptr ptr) with
            true   ->
                      if (L.string_of_lltype (L.type_of e') <> "%matrix_i*" &&
                           L.string_of_lltype (L.type_of e') <> "%matrix_f*" &&
                           L.string_of_lltype (L.type_of e') <> "%matrix_b*" )
                      then raise (Failure "error: matrix must be assigned to a matrix")
                      else
                      (* Semantic checker ensures dimensions must be
                         compatible ie. assignment must be between matrices of same dimensions *)
                      let (mat_typ, lltype, rows, cols) = (match rh_ty with
                          A.Matrix(typ, r, c) -> (match typ with
                                                    A.Int   -> matrix_i, i32_t, r, c
                                                  | A.Float -> matrix_f, float_t, r, c
                                                  | A.Bool  -> matrix_b, i1_t, r, c
                                                  | _       -> raise (Failure "whomp"))
                        | _                   -> raise (Failure "error: invalid assignment"))
                      in
                      (* Assign matrix on RHS to LHS and update hashtable *)
                      let m = stack_build_mat_init ptr rows cols mat_typ lltype expr builder in
                      Hashtbl.add mp s (m, rh_ty);
                      reassign_mat m e' rows cols lltype builder; e'
            (* Assign value normally *)
          | false  -> let _  = L.build_store e' (lookup s) builder in e'
             (* let typ1 = L.string_of_lltype (L.type_of (L.build_load ptr "tmp" builder)) in
             let typ2 = L.string_of_lltype (L.type_of e') in
             if (typ1 <> typ2) then failwith ("Semantic error : type "^typ1^" is assigned with type " ^typ2); *)
        )
    | SCall ("print", [e]) ->
        let e' = expr builder e in
        (match (type_of_lvalue e') with
          A.Int    -> L.build_call printf_func [| int_format_str ; (e') |] "print" builder
        | A.Float  -> L.build_call printf_func [| float_format_str ; (e') |] "printflt" builder
        | A.String -> L.build_call printf_func [| string_format_str ; (e') |] "printstr" builder
        | _        -> raise (Failure "invalid print operation")
        )
    | SCall ("rows", [e]) ->
        let e' = expr builder e in
        let m_row = L.build_load (L.build_struct_gep e' 1 "m_row" builder) "" builder in
        m_row
    | SCall ("cols", [e]) ->
        let e' = expr builder e in
        let m_col = L.build_load (L.build_struct_gep e' 2 "m_col" builder) "" builder in
        m_col
    | SMatLit(mat, r, c) ->
      let (_, sx) = List.hd (List.hd mat) in (match sx with
        | SBoolLit _  -> build_mat_lit mat r c matrix_b i1_t expr builder
        | SIntLit _   -> build_mat_lit mat r c matrix_i i32_t expr builder
        | SFloatLit _ -> build_mat_lit mat r c matrix_f float_t expr builder
        | _ -> raise (Failure "unsupported matrix type"))
    | SMatAccess (id, row, col) ->
        let row = expr builder row in
        let col = expr builder col in
        let typ = lookup_typ id in
        let ptr = lookup id in
        (match (is_matrix_ptr ptr) with
          true      ->
                        let (_, _, _) = (match typ with
                          A.Matrix(ty, rows, cols) -> (match ty with
                                                  A.Int -> i32_t, rows, cols
                                                | A.Float -> float_t, rows, cols
                                                | A.Bool  -> i1_t, rows, cols
                                                | _       -> raise (Failure "impossible")
                                                )
                        | _ -> raise (Failure "invalid identifier type is not matrix"))
                        in
                        let m_mat = L.build_load (L.build_struct_gep (lookup id) 0 "m_mat" builder) "" builder in
                        (* let m_mat_cast = L.build_load (L.build_bitcast m_mat (pointer_t (pointer_t (array_t (array_t lltype c) r))) "m_mat_cast" builder) "m_mat_elem" builder in *)
                        let m_col = L.build_load (L.build_struct_gep (lookup id) 2 "m_col" builder) "" builder in
                        let index = L.build_add col (L.build_mul (m_col) row "tmp" builder) "index" builder in
                        (* let reg = L.build_gep m_mat_cast [| L.const_int i32_t 0; row; col |] id builder in *)
                        let reg = L.build_gep m_mat [| index |] id builder in
                        L.build_load reg id builder
        | false     -> raise (Failure ("invalid matrix access in type " ^ (L.string_of_lltype (L.type_of ptr))))
        )
        (* let reg = L.build_gep (lookup id) [| zero; row; col |] id builder in
        L.build_load reg id builder *)
    | SMatAssign (id, row, col, value) ->
        let row   = expr builder row in
        let col   = expr builder col in
        let typ = lookup_typ id in
        let ptr = lookup id in
        (match (is_matrix_ptr ptr) with
          true      ->
                        let (_, _, _) = (match typ with
                          A.Matrix(ty, rows, cols) -> (match ty with
                                                  A.Int -> i32_t, rows, cols
                                                | A.Float -> float_t, rows, cols
                                                | A.Bool  -> i1_t, rows, cols
                                                | _       -> raise (Failure "impossible")
                                                )
                        | _ -> raise (Failure "invalid identifier type is not matrix"))
                        in
                        let m_mat = L.build_load (L.build_struct_gep (lookup id) 0 "m_mat" builder) "" builder in
                        (* let m_mat_cast = L.build_load (L.build_bitcast m_mat (pointer_t (pointer_t (array_t (array_t lltype c) r))) "m_mat_cast" builder) "m_mat_elem" builder in *)
                        let m_col = L.build_load (L.build_struct_gep (lookup id) 2 "m_col" builder) "" builder in
                        let index = L.build_add col (L.build_mul (m_col) row "tmp" builder) "index" builder in

                        let reg = L.build_gep m_mat [| index |] id builder in
                        let value = expr builder value in
                        L.build_store value reg builder
        | false     -> raise (Failure "invalid matrix access")
        )
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
        let (fdef, fdecl) = lookup_func f function_decls in
        let _ = print_string ("statements : " ^ (String.concat " " (List.map string_of_sexpr act))) in
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in
        (* Flatten matrix pointers into matrix structs *)
        let flatten_actuals a = (match (is_matrix_ptr a) with
          true    -> (L.build_load a "flat_act" builder)
        | false   -> a
          )
        in
        (* If matrix is a formal/actual, update the formal definition to the actual *)
        let update_matrix_actuals a f = (match a with
          (_, SCall(fn, _))   -> let (fndef, fndecl) = lookup_func fn function_decls in (fndecl.styp, (snd f))
        | (A.Matrix(t, r, c), _) -> (match (get_styp f) with
                                      A.Matrix(_, _, _) -> (A.Matrix(t,r,c), (snd f))
                                    | _                    -> raise (Failure "actuals and formals don't map"))
        | _                 -> f
        ) in
        let dynamic_formals = List.map2 (update_matrix_actuals) act fdecl.sformals in
        let _ = print_string ("\nfunction " ^ fdecl.sfname ^ " has BEFORE formals " ^ (String.concat "  "(List.map A.string_of_typ (List.map (fun (x,y) -> x) fdecl.sformals)))) in
        let _ = fdecl.sformals <- dynamic_formals in
        let actuals = List.map (flatten_actuals) actuals in
        let _ = print_string ("\nfunction " ^ fdecl.sfname ^ " has actuals " ^ (String.concat "  "(List.map A.string_of_typ (List.map (fun (x,y) -> x) act)))) in
        let _ = print_string ("\nfunction " ^ fdecl.sfname ^ " has AFTER formals " ^ (String.concat "  "(List.map A.string_of_typ (List.map (fun (x,y) -> x) fdecl.sformals)))) in
        let _ = print_string ("\nfunction " ^ fdecl.sfname ^ " has RETURN " ^ (A.string_of_typ fdecl.styp)) in

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
  | SBreak n  -> let _ = L.build_br (List.nth !break_stack (n - 1)) builder in builder
  | SReturn e -> let _ = (match fdecl.styp with
                          (* Special "return nothing" instr *)
                          A.Void -> L.build_ret_void builder
                          (* Matrix return statements *)
                        | A.Matrix(ty, r, c) -> let _ = fdecl.styp <- (A.Matrix(ty, r, c)) in
                                                let _ = print_string ("\nnew type in function " ^fdecl.sfname ^ " with return "^ A.string_of_typ fdecl.styp) in
                                                (match ty with
                                                  (* A.Int   -> L.build_ret (L.build_load (expr builder e) "return_mat" builder) builder
                                                | A.Float -> L.build_ret (L.build_load (expr builder e) "return_mat" builder) builder
                                                | A.Bool  -> L.build_ret (L.build_load (expr builder e) "return_mat" builder) builder *)
                                                A.Int   -> L.build_ret (L.build_load (expr builder e) "return_mat" builder) builder
                                              | A.Float -> L.build_ret (L.build_load (expr builder e) "return_mat" builder) builder
                                              | A.Bool  -> L.build_ret (L.build_load (expr builder e) "return_mat" builder) builder
                                                | _       -> raise (Failure "error: invalid matrix type"))
                          (* Build return statement *)
                        | _ -> L.build_ret (expr builder e) builder)
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

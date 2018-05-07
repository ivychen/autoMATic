open P_ast
module StringMap = Map.Make (String)

let merge_tbl tbl1 tbl2 =
    (* tbl1 overwrites tbl2 *)
    let opt = (fun _ t1 t2 ->
        match t1, t2 with
          None, None -> assert false
        | Some t1, None -> Some t1
        | None, Some t2 -> Some t2
        | Some t1, Some t2 -> Some t1) in
    StringMap.merge opt tbl1 tbl2

let rec from_file file =
    let ic = open_in file in
    from_channel ic
and from_channel ic =
    let buf = Buffer.create 1024 in
    let ps_key = "#ps" in (* very jank *)
    let eval expr tbl = 
        if not (StringMap.mem ps_key tbl) 
        then match expr with
          End -> StringMap.add ps_key "" tbl
        | _ -> tbl
        else match expr with
          Pass(s) -> Buffer.add_string buf s; tbl
        | Inc(file) -> 
            let included = from_file file in
            let incl_tbl = fst included and incl_src = snd included in
            Buffer.add_string buf incl_src;
            merge_tbl tbl incl_tbl
        | DefInt(name, value) -> StringMap.add name (string_of_int value) tbl
        | DefStrLit(name, value) -> StringMap.add name value tbl
        | DefVar(name1, name2) -> if StringMap.mem name2 tbl
            then StringMap.add name1 (StringMap.find name2 tbl) tbl
            else raise (Failure "preprocessor: undefined variable")
        | Undef(name) -> StringMap.remove name tbl
        | Var(name) -> if StringMap.mem name tbl 
            then Buffer.add_string buf (StringMap.find name tbl)
            else Buffer.add_string buf name; tbl
        | Ifdef(name) -> if StringMap.mem name tbl
            then tbl
            else StringMap.remove ps_key tbl
        | Ifndef(name) -> if StringMap.mem name tbl
            then StringMap.remove ps_key tbl
            else tbl
        | End -> tbl
    in
    let lex_buf = Lexing.from_channel ic in
    let stmts = P_parser.program P_scanner.token lex_buf in
    let init_tbl = StringMap.add ps_key "" StringMap.empty in
    let final_tbl = List.fold_right eval stmts init_tbl in
    (final_tbl, Buffer.contents buf)

(* let _ = print_endline (from_file Sys.argv.(1)) *)

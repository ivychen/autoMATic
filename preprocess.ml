open P_ast
module StringMap = Map.Make (String)

let rec from_file file =
    let ic = open_in file in
    from_channel ic
and from_channel ic =
    let buf = Buffer.create 1024 in
    let eval expr table = 
        match expr with
          Pass(s) -> Buffer.add_string buf s; table
        | Inc(file) -> 
            Buffer.add_string buf (from_file file);
            table
        (* TODO: handle ints as ints *)
        | DefInt(name, value) -> StringMap.add name (string_of_int value) table
        | DefStrLit(name, value) -> StringMap.add name value table
        | DefVar(name1, name2) -> if StringMap.mem name2 table
            then StringMap.add name1 (StringMap.find name2 table) table
            else raise (Failure "undefined variable")
        | Var(name) -> if StringMap.mem name table 
            then Buffer.add_string buf (StringMap.find name table)
            else Buffer.add_string buf name; table
    in
    let lex_buf = Lexing.from_channel ic in
    let stmts = P_parser.program P_scanner.token lex_buf in
    let _ = List.fold_right eval stmts StringMap.empty in
    Buffer.contents buf
(*
let _ =
    print_endline (from_file Sys.argv.(1))
*)

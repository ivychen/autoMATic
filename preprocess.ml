open Ast
open Parser
module StringMap = Map.Make (String)

let print_file file =
    let ic = open_in file in
    try
        while true do
            let line = input_line ic in
            print_endline line
        done
    with End_of_file ->
        close_in ic

let eval expr table = 
    match expr with
      Pass(s) -> print_string s; table
    | Inc(file) -> 
        print_file file (* oc *); 
        print_endline "";
        table
    (* TODO: handle ints as ints *)
    | DefInt(name, value) -> StringMap.add name (string_of_int value) table
    | DefStrLit(name, value) -> StringMap.add name value table
    | DefVar(name1, name2) -> if StringMap.mem name2 table
        then StringMap.add name1 (StringMap.find name2 table) table
        else raise (Failure "undefined variable")
    | Var(name) -> if StringMap.mem name table 
        then print_string (StringMap.find name table)
        else print_string name; table

let _ = 
    let module StringMap = Map.Make (String) in
    let lex_buf = Lexing.from_channel stdin in
    let stmts = Parser.program Scanner.token lex_buf in
    (* List.iter debug (List.rev stmts) *)
    List.fold_right eval stmts StringMap.empty
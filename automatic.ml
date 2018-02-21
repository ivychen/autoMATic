(* Top-level of the autoMATic compiler:
 * scan & parse the input, pretty-print the AST*)

let () =
  let usage_msg = "usage: ./automatic.native [file.ic]" in
  let channel = ref stdin in
  Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in  
  print_string (Ast.string_of_program ast)

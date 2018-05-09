(* autoMATic Compiler - Spring 2018 - COMS4115 PLT
 by Ivy Chen ic2389, Nguyen Chi Dung ncd2118,
 Nelson Gomez ng2573, Jimmy O'Donnell jo2474 *)

(*              _        __  __       _______ _
               | |      |  \/  |   /\|__   __(_)
     __ _ _   _| |_ ___ | \  / |  /  \  | |   _  ___
    / _` | | | | __/ _ \| |\/| | / /\ \ | |  | |/ __|
   | (_| | |_| | || (_) | |  | |/ ____ \| |  | | (__
    \__,_|\__,_|\__\___/|_|  |_/_/    \_\_|  |_|\___|
   A matrix manipulation language that might not suck

This is the top-level of the autoMATic compiler. It will:
 - run the preprocessor and perform text replacement
 - scan & parse the modified input
 - check the resulting AST and generate an SAST from it
 - and generate LLVM IR and dump the module.
*)

type action = Preprocess | Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-p", Arg.Unit (set_action Preprocess), "Print the preprocessed source file");
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./automatic.native [-p|-a|-s|-l|-c] [file.ic]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let preproc = Preprocess.from_channel !channel in
  let preproc_src = snd preproc in
  let lexbuf = Lexing.from_string preproc_src in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Preprocess -> print_string preproc_src
  | Ast -> print_string (Ast.string_of_program ast)
  | _ -> let sast = Semant.check ast in
    match !action with
      Preprocess -> ()
    | Ast        -> ()
    | Sast       -> print_string (Sast.string_of_sprogram sast)
    | LLVM_IR    -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
    | Compile    -> let m = Codegen.translate sast in
	Llvm_analysis.assert_valid_module m;
	print_string (Llvm.string_of_llmodule m)

(* Testing SAST/SEMANT *)
  (* Deal with command line *)
  (* let usage_msg = "usage: ./automatic.native [file.ic]" in
  let channel = ref stdin in
  Arg.parse [] (fun file -> channel := open_in file) usage_msg;
  (* Invoke compiler *)
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check ast in
  print_string (Sast.string_of_sprogram sast) *)

(* Testing AST/SCANNER/PARSER *)
  (* let usage_msg = "usage: ./automatic.native [file.ic]" in
  let channel = ref stdin in
  Arg.parse [] (fun file -> channel := open_in file) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  print_string (Ast.string_of_program ast) *)

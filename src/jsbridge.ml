module WLex = Whilelexer
module WPar = Whileparser
module Analyzer = Analyzer.Intanalyzer
module R = Analyzer.R

module JsUn = Js.Unsafe

(*  calc_pos : Lexing.lexbuf -> int * int  *)
let calc_pos lexbuf =
  let pos  = lexbuf.Lexing.lex_curr_p in
  let line = pos.Lexing.pos_lnum in
  let col  = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (line,col)

(*  build_error : string -> (int * int) -> js_obj  *)    
let build_error msg (line,col) =
  JsUn.obj [| ("line",   JsUn.inject line);
	      ("column", JsUn.inject col);
	      ("msg",    JsUn.inject (Js.string msg)); |]

(*  parse : js_string t -> js_obj *)
let parse s =
  let lexbuf = Lexing.from_string (Js.to_string s) in
  let error msg = JsUn.obj [| ("error", JsUn.inject (build_error msg (calc_pos lexbuf))) |] in
  try
    match WPar.prog WLex.nexttoken lexbuf with
      | [id1,ast1] -> JsUn.obj [| ("result",    JsUn.inject (Js.string "ok")); |]
      | []         -> error "No process given"
      | _::_       -> error "Too many processes given"
  with
    | Failure msg -> error ("Failure in " ^ msg)
    | End_of_file -> error "Parse error: unexpected end of string"
    | WPar.Error  -> error "Parse error"
    
(*  interpret : js_string t -> js_string t -> js_obj *)
let interpret s s' =
  let () = Last.reset () in
  let () = Ast.reset_info () in
  let lexbuf1 = Lexing.from_string (Js.to_string s) in
  let error1 msg = JsUn.obj [| ("error1", JsUn.inject (build_error msg (calc_pos lexbuf1))) |] in
  try
    match WPar.prog WLex.nexttoken lexbuf1 with
      | [id1,ast1] ->
	let () = Lexing.flush_input lexbuf1 in
	let () = Parsing.clear_parser () in
	let lexbuf2 = Lexing.from_string (Js.to_string s') in
	let error2 msg = JsUn.obj [| ("error2", JsUn.inject (build_error msg (calc_pos lexbuf2))) |] in
	(try
	   (match WPar.prog WLex.nexttoken lexbuf2 with
	     | [id2,ast2] ->
	       let last1 = Last.label (Ast.desugar_proc ast1) in
	       let last2 = Last.label (Ast.desugar_proc ast2) in
	       let () = Format.fprintf Format.str_formatter "Channel name mapping:\n----------------------\n" in
	       let () = Ast.print_info Format.str_formatter in
	       let info = Format.flush_str_formatter () in
	       let _ = Analyzer.eval_twoproc_policy (last1,last2) R.top 5 Format.str_formatter in
	       let s  = Format.flush_str_formatter () in
	       JsUn.obj [| ("result",    JsUn.inject (Js.string (info ^ "\n\n" ^ s))); |]
	     | []   -> error2 "No second process given"
	     | _::_ -> error2 "Too many processes given in field 2")
	 with
	   | Failure msg -> error2 ("Failure in " ^ msg)
	   | End_of_file -> error2 "Parse error: unexpected end of string"
	   | WPar.Error  -> error2 "Parse error in process 2")
      | []   -> error1 "No first process given"
      | _::_ -> error1 "Too many processes given in field 1"
  with
    | Failure msg -> error1 ("Failure in " ^ msg)
    | End_of_file -> error1 "Parse error: unexpected end of string"
    | WPar.Error  -> error1 "Parse error in process 1"

(* export interpret function to JS's global scope *)
let () = Js.Unsafe.global##interpret <- Js.wrap_callback interpret
let () = Js.Unsafe.global##parse     <- Js.wrap_callback parse


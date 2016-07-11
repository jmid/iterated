module A = Ast
module Analyzer = Analyzer.Intanalyzer
module R = Analyzer.R
module R' = Analyzer.R'
module I = Interval

module WLex = Whilelexer
module WPar = Whileparser
module RELexerInfo = Regexplexer.Make(Ast) (* RE lexer parameterized with Ast *)

let parse_pol s = Regexpparser.regexp RELexerInfo.nexttoken (Lexing.from_string s)
 
(*  calc_pos : Lexing.lexbuf -> int * int  *)
let calc_pos lexbuf =
  let pos  = lexbuf.Lexing.lex_curr_p in
  let line = pos.Lexing.pos_lnum in
  let col  = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (line,col)

(*  parse_lexbuf : -> string -> Lexing.lexbuf -> (unit -> unit) -> Ast.prog  *)
let parse_lexbuf f lexbuf close =
  try WPar.prog WLex.nexttoken lexbuf 
  with Parsing.Parse_error -> 
    let line,col = calc_pos lexbuf in
    begin
      Printf.printf " Parse error in %s line %i, column %i\n" f line col;
      close();
      if !Sys.interactive then raise Parsing.Parse_error else exit(1);
    end

(*  parse_str : string -> Ast.stmt  *)
let parse_str s = parse_lexbuf "input string" (Lexing.from_string s) (fun () -> ())

(*  parse_file : string -> Ast.stmt  *)
let parse_file f =
  let ch  = open_in f in
  let stm = parse_lexbuf f (Lexing.from_channel ch) (fun () -> close_in ch) in
  let ()  = close_in ch in
  stm

(*  parse_one : string -> Ast.stmt  *)
let parse_one str = match parse_str str with
  | (_,stmt)::_ -> stmt
  | _           -> failwith "parse_one did not receive a process"

(*  eval_str_pp : string -> unit  *)
let eval_str_pp str = match parse_str str with
  | (_,s)::_ ->
    let () = Last.reset () in
    let () = Ast.reset_info () in
    let ast  = Ast.desugar_proc s in
    let last = Last.label ast in
    let () = Ast.print_info Format.std_formatter in
    Analyzer.eval_proc_top ~pp:true last
  | _ -> failwith "eval_str_pp did not receive a process"

(*  eval_str_policy_pp : string -> string -> unit  *)
let eval_str_policy_pp str pol = match parse_str str with
  | (_,s)::_ ->
    let () = Last.reset () in
    let () = Ast.reset_info () in
    let ast  = Ast.desugar_proc s in
    let last = Last.label ast in
    let pol  = parse_pol pol in
    let () = Ast.print_info Format.std_formatter in
    Analyzer.eval_proc_policy ~pp:true last pol Format.std_formatter
  | _ -> failwith "eval_str_policy_pp did not receive a process"

(*  eval_file_pp : string -> unit  *)
let eval_file_pp f = match parse_file f with
  | (_,s)::_ ->
    let () = Last.reset () in
    let () = Ast.reset_info () in
    let ast  = Ast.desugar_proc s in
    let last = Last.label ast in
    let () = Ast.print_info Format.std_formatter in
    Analyzer.eval_proc_top ~pp:true last Format.std_formatter
  | _ -> failwith "eval_file_pp did not receive a process"

(*  eval_file_policy_pp : string -> string -> unit  *)
let eval_file_policy_pp f pol = match parse_file f with
  | (_,s)::_ ->
    let () = Last.reset () in
    let () = Ast.reset_info () in
    let ast  = Ast.desugar_proc s in
    let last = Last.label ast in
    let pol  = parse_pol pol in
    let () = Ast.print_info Format.std_formatter in
    Analyzer.eval_proc_policy ~pp:true last pol Format.std_formatter
  | _ -> failwith "eval_file_policy_pp did not receive a process"

(*  eval_str_twoproc_pp : string -> int -> unit  *)
let eval_str_twoproc_pp str rounds = match parse_str str with
  | (_,s)::(_,s')::_ ->
    let () = Last.reset () in
    let () = Ast.reset_info () in
    let last  = Last.label (Ast.desugar_proc s) in
    let last' = Last.label (Ast.desugar_proc s') in
    let () = Ast.print_info Format.std_formatter in
    Analyzer.eval_twoproc_policy (last,last') R.top rounds Format.std_formatter
  | _ -> failwith "eval_str_twoproc_pp did not receive two processes"
    
(*  eval_file_twoproc_pp : string -> int -> unit  *)
let eval_file_twoproc_pp f rounds = match parse_file f with
  | (_,s)::(_,s')::_ ->
    let () = Last.reset () in
    let () = Ast.reset_info () in
    let last  = Last.label (Ast.desugar_proc s) in
    let last' = Last.label (Ast.desugar_proc s') in
    let () = Ast.print_info Format.std_formatter in
    Analyzer.eval_twoproc_policy (last,last') R.top rounds Format.std_formatter
  | _ -> failwith "eval_file_twoproc_pp did not receive two processes"

(*  eval_twoproc_pp : exstmt * exstmt -> int -> unit  *)
let eval_twoproc_pp (s,s') rounds =
  let () = Last.reset () in
  let () = Ast.reset_info () in
  let last  = Last.label (Ast.desugar_proc s) in
  let last' = Last.label (Ast.desugar_proc s') in
  let () = Ast.print_info Format.std_formatter in
  Analyzer.eval_twoproc_policy (last,last') R.top rounds Format.std_formatter
    
let swap (p,p') = (p',p)
let pp = ref false (*false*)
let time = ref true (*true*)
let examples = (* a number of examples with an (optional) list of policies *)
  [ ("Ast.deadlock",   A.deadlock);
    ("Ast.cc80const",  A.cc80const);
    ("Ast.cc80",        A.cc80);
    ("Ast.counter",     A.counter);
    ("Ast.highscore 0", A.highscore 0);
    ("Ast.highscore 0 (swapped)", swap (A.highscore 0));
    ("Ast.math",        A.math);
    ("Ast.math_alt",    A.math_alt);
    ("Ast.math2",       A.math2);
    ("Ast.zwrcb80",     A.zwrcb80);
    ("Ast.zwrcb80 (swapped)",     swap A.zwrcb80);
    ("Ast.zwrcb80'",    A.zwrcb80');
    ("Ast.zwrcb80' (swapped)",    swap A.zwrcb80');
    ("Ast.simplerclientserver, cap 2", A.simplerclientserver 0 2);
    ("Ast.simplerclientserver, cap 2 (swapped)", swap (A.simplerclientserver 0 2));
    ("Ast.clientserver, cap 2", A.clientserver 0 2);
    ("Ast.clientserver, cap 2 (swapped)", swap (A.clientserver 0 2));
  ] 

module A = Ast
module LA = Last

module Make(Astore :
            sig
	      module Val : sig
		             include Regexpdom.EXTLATTICE
			     val const : int -> elem
	                   end
	      include Absstore.LATTICE
	      val empty           : elem
              val extend          : elem -> Ast.var -> Val.elem -> elem
	      val eval_aexp       : Ast.aexp -> elem -> Val.elem
	      val eval_bexp_true  : Ast.bexp -> elem -> elem
	      val eval_bexp_false : Ast.bexp -> elem -> elem
            end)  =
struct
  module Val = Astore.Val
  module Channame = struct
                      include Interval
		      let widening = join (* only finitely many channel names, widening not needed *)
                    end
  module ReadPair = Redpairdom.MakeExt(Channame)(Val)(struct let prefix = "?" end)
  module WritePair = Redpairdom.MakeExt(Channame)(Val)(struct let prefix = "!" end)
  module Chandom = struct
                     include Pairdom.MakeExt(ReadPair)(WritePair)
                     (*  fpprint : Format.formatter -> elem -> unit  *)
		     let fpprint fmt (c1,c2) =
		       if leq top (c1,c2)
		       then
			 Format.fprintf fmt "Top"
		       else
			 let s1 = ReadPair.to_string c1 in
			 let s2 = WritePair.to_string c2 in
 		         match ReadPair.leq c1 ReadPair.bot, WritePair.leq c2 WritePair.bot with
			   | true,false  -> Format.fprintf fmt "%s" s2
			   | false,true  -> Format.fprintf fmt "%s" s1
			   | true,true
			   | false,false -> Format.fprintf fmt "(%s, %s)" s1 s2  

                     (*  pprint : elem -> unit  *)
		     let pprint e =
		       begin
			 fpprint Format.std_formatter e;
			 Format.print_flush ();
		       end

                     (*  to_string : L.elem -> string  *)
		     let to_string (c1,c2) =
		       if leq top (c1,c2)
		       then "Top"
		       else
			 let s1 = ReadPair.to_string c1 in
			 let s2 = WritePair.to_string c2 in
 		         match ReadPair.leq c1 ReadPair.bot, WritePair.leq c2 WritePair.bot with
			   | true,false  -> s2
			   | false,true  -> s1
			   | true,true
			   | false,false -> "(" ^ s1 ^ ", " ^ s2 ^ ")"
  end
  module R = struct
               include Regexpdom.Make(Chandom)
	       let widening r r' = widening r r'
(*  Other widening choices:  *)
(*		let widening r r' = (rev (widening (rev r) (rev r'))) *)
(*		let widening r r' = determinize (widening r r') *)
(*		let widening r r' = determinize (rev (widening (rev r) (rev r'))) *)
(*		let pprint r = pprint (determinize r) *)
             end
  module R' = struct
                include Regexpdom.Make(Chandom)
		let widening = join
              end

  let writetag ch v = Chandom.pair (ReadPair.bot, WritePair.pair(Channame.const ch, v))
  let readtag ch v = Chandom.pair (ReadPair.pair(Channame.const ch, v), WritePair.bot)

  let pp = ref false

  (*  time_apply : ('a -> 'b) -> 'a -> 'b  *)
  let time_apply f x = f x (* uncommented to make it compile with js_of_ocaml *)
(*  let start = Unix.gettimeofday () in
    let res = f x in
    let stop = Unix.gettimeofday () in
    let () = Printf.printf "Execution time: %fs\n%!" (stop -. start) in
    res *)

  module Proddom = Proddom.Make(Astore)(R)(R') (*(R')(R)*)
  module Cache = Cache.Make(Proddom)

  let lfp init func =
    let rec postfix prev =
      let next = func prev in
      let next' = (Cache.widening (fst prev) (fst next),
		   Cache.widening (snd prev) (snd next)) in
      let () = if !pp then
	  begin
	    Format.printf "prev: ";
	    Cache.pprint (fst prev); Format.printf ", "; Cache.pprint (snd prev);
            Format.printf "  next: ";
	    Cache.pprint (fst next); Format.printf ", "; Cache.pprint (snd next);
            Format.printf "  next': ";
	    Cache.pprint (fst next'); Format.printf ", "; Cache.pprint (snd next');
            Format.printf "\n";
	  end else () in
      if Cache.leq (fst next') (fst prev) && Cache.leq (snd next') (snd prev) then prev else postfix next'
    in
    let rec narrow prev =
      let next = func prev in
      let next' = (Cache.narrowing (fst prev) (fst next),
		   Cache.narrowing (snd prev) (snd next)) in
      let () = if !pp then
	  begin
	    Format.printf "prev: ";
	    Cache.pprint (fst prev); Format.printf ", "; Cache.pprint (snd prev);
            Format.printf "  next: ";
	    Cache.pprint (fst next); Format.printf ", "; Cache.pprint (snd next);
            Format.printf "  next': ";
	    Cache.pprint (fst next'); Format.printf ", "; Cache.pprint (snd next');
            Format.printf "\n";
	  end else () in
      if Cache.eq (fst next') (fst prev) && Cache.eq (snd next') (snd prev) then prev else narrow next'
    in
    narrow (postfix init)

  (* ************************************************************ *)
  (*        Version with histories + environment policies         *)
  (* ************************************************************ *)

  let absassign sigma x v = Astore.extend sigma x v

  (*  eval_proc : Ast.stmt -> cache * cache -> cache * cache  *)
  let rec eval_proc s (en,ex) = match s with
    | LA.Skip l         -> (en, Cache.extend ex l (Cache.lookup en l))
    | LA.Assign (l,x,a) ->
      let (sigma,h,f) = Cache.lookup en l in
      (en, Cache.extend ex l (absassign sigma x (Astore.eval_aexp a sigma),h,f))
    | LA.Seq (s1,s2)    ->
      let (en',ex') = eval_proc s1 (en,ex) in
      let labels = LA.last s1 in
      let join = List.fold_left (fun acc l -> Proddom.join acc (Cache.lookup ex' l)) Proddom.bot labels in
      eval_proc s2 (Cache.extend en' (LA.first s2) join,ex')
    | LA.If (l,b,s1,s2) ->
      let (sigma,h,f) = Cache.lookup en l in
      let (en1,ex1) = eval_proc s1 (Cache.extend en (LA.first s1) (Astore.eval_bexp_true b sigma,h,f),ex) in
      let (en2,ex2) = eval_proc s2 (Cache.extend en (LA.first s2) (Astore.eval_bexp_false b sigma,h,f),ex) in
      let res = List.fold_left
  	               (fun acc l -> Proddom.join acc (Cache.lookup ex1 l)) Proddom.bot (LA.last s1) in
      let res = List.fold_left (fun acc l -> Proddom.join acc (Cache.lookup ex2 l)) res (LA.last s2) in
      (Cache.join en1 en2, Cache.extend (Cache.join ex1 ex2) l res)
    | LA.While (l,b,s1)   ->
      (* old widening trick: analyze loop under Eps history then prepend h afterwards *)
(*    let (sigma',h',f') =
	lfp (sigma,Regexpdom.Eps,f) (fun (sigma,h,f) -> eval_proc s (Astore.eval_bexp_true b sigma,h,f)) in
      (Astore.eval_bexp_false b sigma',R.concat (h,h'),f') *)
      let (en',ex') =
	lfp (en,ex)
	  (fun (en,ex) ->
	    let (sigma,h,f) = Cache.lookup en l in
	    let res =
	      List.fold_left (fun acc l1 -> Proddom.join acc (Cache.lookup ex l1)) Proddom.bot (LA.last s1) in
	    eval_proc s1 (Cache.extend
			   (Cache.extend en (LA.first s1) (Astore.eval_bexp_true b sigma,h,f))
			      l res, ex)) in
      let (sigma',h',f') = Cache.lookup en' l in
      (en', Cache.extend ex' l (Astore.eval_bexp_false b sigma',h',f'))
    | LA.Chread (l,ch,x)  ->
      let (sigma,h,f) = Cache.lookup en l in
      let rng = R.range f in
      let res =
	Chandom.fold_partition
	  (fun acc eqcl ->
	    match eqcl with
	      | Chandom.FstEqCl _   -> acc (* read *)
	      | Chandom.SndEqCl eq2 ->     (* write *)
		let chrep,valrep = WritePair.repr eq2 in
		let chprj,valprj = WritePair.project eq2 in
		let d_valrep_f = R.d (writetag ch valrep) f in
		if Channame.leq (Channame.const ch) chprj (* input concerns 'ch' *)
		  && not (R.leq d_valrep_f R.bot) (* and represents a possible future *)
		then
		  Proddom.join acc
		    (Astore.extend sigma x valprj,
		     R.concat (h, R.letter (readtag ch valprj)),
		     d_valrep_f)
		else acc)
	  (Astore.bot,R.bot,R.bot) rng in
      (en, Cache.extend ex l res)
    | LA.Chwrite (l,ch,a) ->
      let (sigma,h,f) = Cache.lookup en l in
      let v' = Astore.eval_aexp a sigma in
      let rng = (R.range f) in
      (*let rng = Chandom.overlay_partitions (R.range f) (Chandom.partition (readtag ch v')) in*)
      let res =
	Chandom.fold_partition
	  (fun acc eqcl ->
	    match eqcl with
	      | Chandom.FstEqCl eq1 ->      (* read *)
		let chrep,valrep = ReadPair.repr eq1 in
		let chprj,valprj = ReadPair.project eq1 in
		let vvmeet = Val.meet valprj v' in
		let d_valrep_f = R.d (readtag ch valrep) f in
		if Channame.leq (Channame.const ch) chprj (* output concerns 'ch' *)
	          && not (Val.leq vvmeet Val.bot)
		  && not (R.leq d_valrep_f R.bot) (* and represents a possible future *)
		then
		  Proddom.join acc
	            (sigma,
		     R.concat (h, R.letter (writetag ch vvmeet)),
		     d_valrep_f)
		else acc
	      | Chandom.SndEqCl _   -> acc) (* write *)
	  (Astore.bot,R.bot,R.bot) rng in
      (en,Cache.extend ex l res)
    | LA.Choice (l,s1,s2) ->
      let lentry = Cache.lookup en l in
      let (en1,ex1) = eval_proc s1 (Cache.extend en (LA.first s1) lentry,ex) in
      let (en2,ex2) = eval_proc s2 (Cache.extend en (LA.first s2) lentry,ex) in

      let res = List.fold_left
  	               (fun acc l -> Proddom.join acc (Cache.lookup ex1 l)) Proddom.bot (LA.last s1) in
      let res = List.fold_left (fun acc l -> Proddom.join acc (Cache.lookup ex2 l)) res (LA.last s2) in
      (Cache.join en1 en2, Cache.extend (Cache.join ex1 ex2) l res)
    | LA.Stop l           ->
      let (sigma,h,f) = Cache.lookup en l in
      (en, Cache.extend ex l (Astore.bot,h,f))

(** ppaexp, ppaterm, and ppafactor implements an expression
    pretty printer following the below BNF grammar *)
(*   expr ::= term
            | expr + term
            | expr - term
     term ::= factor
            | term * factor |
   factor ::= Var x
            | Num i
            | Any
            | ( expr )      *)
  let rec ppaexp fmt e = match e with
    | A.Binop (e1,A.Plus,e2) ->
      begin
	ppaexp fmt e1;
	Format.fprintf fmt " + ";
	ppaterm fmt e2;
      end
    | A.Binop (e1,A.Minus,e2) ->
      begin
	ppaexp fmt e1;
	Format.fprintf fmt " - ";
	ppaterm fmt e2;
      end
    | _ -> ppaterm fmt e
  and ppaterm fmt e = match e with
    | A.Binop (e1,A.Mult,e2) ->
      begin
	ppaterm fmt e1;
	Format.fprintf fmt " * ";
	ppafactor fmt e2;
      end
    | _ -> ppafactor fmt e
  and ppafactor fmt e = match e with
    | A.Num n -> Format.fprintf fmt "%i" n
    | A.Var x -> Format.fprintf fmt "%s" x
    | A.Any   -> Format.fprintf fmt "?"
    | A.Binop (_,_,_) ->
      begin
	Format.fprintf fmt "(";
	ppaexp fmt e;
	Format.fprintf fmt ")";
      end

  (*  ppbexp : Format.formatter -> Ast.aexp -> unit  *)
  let rec ppbexp fmt e = match e with
    | A.True   -> Format.fprintf fmt "true"
    | A.False  -> Format.fprintf fmt "false"
    | A.Not e' ->
      begin
	Format.fprintf fmt "not (";
	ppbexp fmt e';
	Format.fprintf fmt ")";
      end
    | A.Relop (e1,op,e2) ->
      let str = match op with
	| A.Eq  -> "=="
	| A.Lt  -> "<"
	| A.Leq -> "<=" in
      begin
	ppaexp fmt e1;
	Format.fprintf fmt " %s " str;
	ppaexp fmt e2;
      end
    | A.Conj (e1,e2) ->
      begin
	Format.fprintf fmt "(";
	ppbexp fmt e1;
	Format.fprintf fmt ") and ("; (* conj's are still fully parenthesized though *)
	ppbexp fmt e2;
	Format.fprintf fmt ")";
      end

  let fmt_str : ('a -> 'b -> 'c, Format.formatter, unit) format = "%s%30s"

  (*  ppstmt : Format.formatter -> Ast.aexp -> cache * cache -> unit  *)
  let rec ppstmt fmt s caches =
    let (en,ex) = caches in
    match s with
    | LA.Skip l         ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt fmt_str "skip;" "";
	Proddom.fpprint fmt (Cache.lookup ex l);
      end
    | LA.Assign (l,x,e) ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt "%s = " x;
	ppaexp fmt e;
	Format.fprintf fmt fmt_str ";" "";
	Proddom.fpprint fmt (Cache.lookup ex l);
      end
    | LA.Seq (s1,s2)  ->
      begin
	Format.fprintf fmt "@[<v 0>";
	ppstmt fmt s1 caches;
	Format.pp_print_space fmt ();
	ppstmt fmt s2 caches;
	Format.fprintf fmt "@]";
      end
    | LA.While (l,e,s')  ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt "@[<v 0>@[<v 2>while (";
	ppbexp fmt e;
	Format.fprintf fmt fmt_str ") {" "";
	Proddom.fpprint fmt (Cache.lookup en (LA.first s'));
	Format.pp_print_space fmt ();
	ppstmt fmt s' caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt fmt_str "}" "";
	Proddom.fpprint fmt (Cache.lookup ex l);
	Format.fprintf fmt "@]";
      end
    | LA.If (l,e,s1,s2)  ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt "@[<v 0>if (";
	ppbexp fmt e;
	Format.fprintf fmt ")";
	Format.pp_print_space fmt ();
	Format.fprintf fmt "@[<v 2>then {";
	Format.pp_print_space fmt ();
	ppstmt fmt s1 caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt "@[<v 2>} else {";
	Format.pp_print_space fmt ();
	ppstmt fmt s2 caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt fmt_str "}" "";
	Proddom.fpprint fmt (Cache.lookup ex l);
	Format.fprintf fmt "@]";
      end
    | LA.Chread (l,ch,x) ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt "%i? %s;" ch x;
	Format.fprintf fmt fmt_str "" "";
	Proddom.fpprint fmt (Cache.lookup ex l);
      end
    | LA.Chwrite (l,ch,e) ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt "%i! " ch;
	ppaexp fmt e;
	Format.fprintf fmt fmt_str ";" "";
	Proddom.fpprint fmt (Cache.lookup ex l);
      end
    | LA.Stop l        ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt fmt_str "stop;" "";
	Proddom.fpprint fmt (Cache.lookup ex l);
      end
    | LA.Choice (l,s1,s2) ->
      begin
	Format.fprintf fmt "%i:" l;
	Format.fprintf fmt "@[<v 0>@[<v 2>choose {";
	Format.pp_print_space fmt ();
	ppstmt fmt s1 caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt "@[<v 2>} | {";
	Format.pp_print_space fmt ();
	ppstmt fmt s2 caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt "}";
	Format.fprintf fmt "@]";
      end
	
  (*  eval_proc_policy : Last.stmt -> R.elem -> Format.formatter -> (Cache * Cache) * R.elem  *)
  let eval_proc_policy ?(pp=true) ?(time=false) ls pol fmt =
    let apply = if time then time_apply else fun f x -> f x in
    let pair = (Cache.extend Cache.empty (LA.first ls) (Astore.empty,Regexpdom.Eps,pol),
		Cache.empty) in
    let res = apply (eval_proc ls) pair in
    let _,coll_h',_  = Cache.fold (fun i triple acc -> Proddom.join triple acc) (fst res) in 
    if pp
    then
      begin
	Format.pp_print_space fmt ();
	Format.fprintf fmt fmt_str "" "";
	Proddom.fpprint fmt (Cache.lookup (fst res) (LA.first ls));
	Format.pp_print_newline fmt ();
	ppstmt fmt ls res;
	Format.pp_print_space fmt ();
	Format.pp_print_space fmt ();
	Format.fprintf fmt "\nCollective prefix': ";
	R.fpprint fmt coll_h';
	Format.pp_print_newline fmt ();
	Format.pp_print_newline fmt ();
	res, coll_h'
      end
    else res, coll_h'

  (*  eval_proc_top : Ast.stmt -> Format.formatter -> unit  *)
  let eval_proc_top ?(pp=true) ?(time=false) s fmt = eval_proc_policy ~pp ~time s R.top fmt

  (*  eval_twoproc_policy : (Last.stmt * Last.stmt) -> R.elem -> -> int -> Format.formatter -> unit  *)
  let rec eval_twoproc_policy ?(pp=true) ?(time=false) (s,s') pol max fmt =
    let rec loop pol i =
      if i < max
      then
	let () = Format.fprintf fmt "Iteration %i\n------------" i in
	let r,h   = eval_proc_policy s pol fmt in
	let r',h' = eval_proc_policy s' h fmt in
	(if R.leq pol h'
	 then Format.fprintf fmt "Reached fixed point, bailing early\n"
	 else loop h' (succ i))
      else ()
    in
    loop pol 0
end

module Parityanalyzer = Make(Paritystore)
module Signanalyzer = Make(Signstore)
module Constanalyzer = Make(Conststore)
module Modstore = Modstore.Make(struct let n = 8 end)
module Modanalyzer  = Make(Modstore)
module Intanalyzer  = Make(Intstore)

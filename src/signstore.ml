module A = Ast

module Val = Sign
include Absstore.Make(Sign)

(*  eval_aexp : Ast.aexp -> absstore -> Sign.elem  *)
let rec eval_aexp e sigma = match e with
  | A.Num n  -> Val.const n
  | A.Var x  -> lookup sigma x
  | A.Any    -> Val.top
  | A.Binop (e1,op,e2) ->
    (match eval_aexp e1 sigma, op, eval_aexp e2 sigma with
      | v1, A.Plus,  v2 -> Val.add v1 v2
      | v1, A.Minus, v2 -> Val.sub v1 v2
      | v1, A.Mult,  v2 -> Val.mult v1 v2)

(*  eval_bexp_true : Ast.bexp -> absstore -> absstore  *)
let rec eval_bexp_true b sigma = match b with
  | A.True  -> sigma
  | A.False -> bot
  | A.Not b -> eval_bexp_false b sigma
  | A.Relop (e1,op,e2) ->
    (match eval_aexp e1 sigma, op, eval_aexp e2 sigma with
      | v1, A.Eq, v2 ->
	let meet_v1v2 = Val.meet v1 v2 in
	if Val.is_bot meet_v1v2
        then bot
	else
	  (match e1, e2 with
	    | A.Var x1, A.Var x2 -> extend (extend sigma x1 meet_v1v2) x2 meet_v1v2
	    | A.Var x1,        _ -> extend sigma x1 meet_v1v2
	    |        _, A.Var x2 -> extend sigma x2 meet_v1v2
	    | _, _ -> sigma)
      | v1, A.Lt, v2 ->
	(match v1,v2 with
	  | Sign.Sign 0x00, _    (* Bot *)
	  | _, Sign.Sign 0x00 -> (* Bot *) bot
	  | Sign.Sign 0x01, _ -> (* Neg *) sigma 
	  | Sign.Sign 0x02, _    (* Zero *)
	  | Sign.Sign 0x04, _    (* Pos *)
	  | Sign.Sign 0x06, _ -> (* PosZero *)
	    (match v2 with
	      | Sign.Sign 0x01 (* Neg *)
	      | Sign.Sign 0x02 (* Zero *)
	      | Sign.Sign 0x03 (* NegZero *) -> bot
	      | Sign.Sign 0x04 (* Pos *)     -> sigma
	      | _ (*NotZero,PosZero,Top*)    ->
		(match e2 with
		  | A.Var x2 -> extend sigma x2 (Sign.Sign 0x04) (* x2 : Pos *)
		  | _        -> sigma))
	  | Sign.Sign 0x03, _    (* NegZero *)
	  | Sign.Sign 0x05, _    (* NotZero *)
	  | Sign.Sign 0x07, _ -> (* Top *)
	    (match v2 with
	      | Sign.Sign 0x01 (* Neg *)
	      | Sign.Sign 0x02 (* Zero *)
	      | Sign.Sign 0x03 (* NegZero *) ->
		(match e2 with
		  | A.Var x2 -> extend sigma x2 (Sign.Sign 0x01) (* x2 : Neg *)
		  | _        -> sigma)
	      | _ (*Pos,NotZero,PosZero,Top*)-> sigma)
	  | _, _ ->
	    failwith
	      ("analyzer.ml: Unknown v1 sign encountered: " ^ (Sign.to_string v1)
	                                                    ^ " in eval_bexp_true"))
      | v1, A.Leq, v2 ->
	(match v1,v2 with
	  | Sign.Sign 0x00, _    (* Bot *)
	  | _, Sign.Sign 0x00 -> (* Bot *) bot
	  | Sign.Sign 0x01, _    (* Neg *)
	  | Sign.Sign 0x03, _ -> (* NegZero *)
	    (match v2 with
	      | Sign.Sign 0x01 (* Neg *) ->
		(match e1 with
		  | A.Var x1 -> extend sigma x1 (Sign.Sign 0x01) (* x1 : Neg *)
		  | _        -> sigma)
	      | _                        -> sigma )
	  | Sign.Sign 0x02, _    (* Zero *)
	  | Sign.Sign 0x06, _ -> (* PosZero *)
	    (match v2 with
	      | Sign.Sign 0x01 (* Neg *)  -> bot
	      | Sign.Sign 0x02 (* Zero *) ->
		(match e1 with
		  | A.Var x1 -> extend sigma x1 (Sign.Sign 0x02) (* x1 : Zero *)
		  | _        -> sigma)
	      | Sign.Sign 0x03 (* NegZero *) ->
		(match e1, e2 with
		  | A.Var x1, A.Var x2 ->
		    extend (extend sigma x1 (Sign.Sign 0x02)) x2 (Sign.Sign 0x02) (* x1,x2:Zero *)
		  | A.Var x1,        _ -> extend sigma x1 (Sign.Sign 0x02)        (* x1 : Zero *)
		  |        _, A.Var x2 -> extend sigma x2 (Sign.Sign 0x02)        (* x2 : Zero *)
		  | _        -> sigma)
	      | Sign.Sign 0x04 (* Pos *)     
	      | Sign.Sign 0x06 (* PosZero *) -> sigma
	      | Sign.Sign 0x05 (* NotZero *) ->
		(match e2 with
		  | A.Var x2 -> extend sigma x2 (Sign.Sign 0x04) (* x2 : Pos *)
		  | _        -> sigma)
	      | _              (* Top *)     ->
		(match e2 with
		  | A.Var x2 -> extend sigma x2 (Sign.Sign 0x06) (* x2 : PosZero *)
		  | _        -> sigma))
	  | Sign.Sign 0x04, _ -> (* Pos *)
	    (match v2 with
	      | Sign.Sign 0x01 (* Neg *)
	      | Sign.Sign 0x02 (* Zero *)
	      | Sign.Sign 0x03 (* NegZero *) -> bot
	      | _ (*Pos,NotZero,PosZero,Top*)->
		(match e2 with
		  | A.Var x2 -> extend sigma x2 (Sign.Sign 0x04) (* x2 : Pos *)
		  | _        -> sigma))
	  | Sign.Sign 0x05, _ -> (* NotZero *)
	    (match v2 with
	      | Sign.Sign 0x01 (* Neg *)
	      | Sign.Sign 0x02 (* Zero *)
	      | Sign.Sign 0x03 (* NegZero *) ->
		(match e1 with
		  | A.Var x1 -> extend sigma x1 (Sign.Sign 0x01) (* x1 : Neg *)
		  | _        -> sigma)
	      | _ (*Pos,NotZero,PosZero,Top*)-> sigma)
	  | _, _ ->            (* Top *)
	    (match v2 with
	      | Sign.Sign 0x01 (* Neg *)     ->
		(match e1 with
		  | A.Var x1 -> extend sigma x1 (Sign.Sign 0x01) (* x1 : Neg *)
		  | _        -> sigma)
	      | Sign.Sign 0x02 (* Zero *)
	      | Sign.Sign 0x03 (* NegZero *) ->
		(match e1 with
		  | A.Var x1 -> extend sigma x1 (Sign.Sign 0x03) (* x1 : NegZero *)
		  | _        -> sigma)
	      | _ (*Pos,NotZero,PosZero,Top*)-> sigma) ) )
  | A.Conj (b1,b2) -> eval_bexp_true b2 (eval_bexp_true b1 sigma) (* models left-to-right eval *)

(*  eval_bexp_false : Ast.bexp -> absstore -> absstore  *)
and eval_bexp_false b sigma = match b with
  | A.True  -> bot
  | A.False -> sigma
  | A.Not b -> eval_bexp_true b sigma
  | A.Relop (e1,op,e2) ->
    (match eval_aexp e1 sigma, op, eval_aexp e2 sigma with
      | v1, A.Eq, v2  -> if Val.is_bot v1 || Val.is_bot v2 then bot else sigma
      | _, A.Lt, _    -> eval_bexp_true (A.Relop (e2,A.Leq,e1)) sigma
      | _, A.Leq, _   -> eval_bexp_true (A.Relop (e2,A.Lt,e1)) sigma )
  | A.Conj (b1,b2) -> join (eval_bexp_false b1 sigma) (* models shortcut Bool eval *)
                           (eval_bexp_false b2 (eval_bexp_true b1 sigma))


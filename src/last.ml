module A = Ast

type label = int
type stmt =
  | Skip of label
  | Assign of label * A.var * A.aexp
  | Seq of stmt * stmt
  | While of label * A.bexp * stmt
  | If of label * A.bexp * stmt * stmt
  | Chread of label * A.chan * A.var (*var list*)
  | Chwrite of label * A.chan * A.aexp (*var list*)
  | Choice of label * stmt * stmt
  | Stop of label

let reset,make_label =
  let count     = ref 1 in
  (fun () -> count := 1),
  (fun () -> let label = !count in
	     begin 
	       incr count;
	       label
	     end)

let rec label s = match s with
  | A.Skip           -> Skip (make_label ())
  | A.Assign (x,e)   -> Assign (make_label (),x,e)
  | A.Seq (s1,s2)    ->
    let s1' = label s1 in
    let s2' = label s2 in
    Seq (s1',s2')
  | A.While (e,s)    ->
    let l  = make_label () in
    let s' = label s in
    While (l,e,s')
  | A.If (e,s1,s2)   ->
    let l   = make_label () in
    let s1' = label s1 in
    let s2' = label s2 in
    If (l,e,s1',s2')
  | A.Chread (ch,x)  -> Chread (make_label (),ch,x)
  | A.Chwrite (ch,e) -> Chwrite (make_label (),ch,e)
  | A.Choice (s1,s2) ->
    let l   = make_label () in
    let s1' = label s1 in
    let s2' = label s2 in
    Choice (l,s1',s2')
  | A.Stop           -> Stop (make_label ())

(*  first : stmt -> label  *)
let rec first s = match s with
  | Skip l          -> l
  | Assign (l,_,_)  -> l
  | Seq (s1,s2)     -> first s1
  | If (l,_,_,_)    -> l
  | While (l,_,_)   -> l
  | Choice (l,_,_)  -> l
  | Chread (l,_,_)  -> l
  | Chwrite (l,_,_) -> l
  | Stop l          -> l

(*  last : stmt -> label list  *)
let rec last s = match s with
  | Skip l           -> [l]
  | Assign (l,_,_)   -> [l]
  | Seq (s1,s2)      -> last s2
  | If (_,_,s1,s2)   -> (last s1) @ (last s2)
  | While (l,_,_)    -> [l]
  | Choice (_,s1,s2) -> (last s1) @ (last s2)
  | Chread (l,_,_)   -> [l]
  | Chwrite (l,_,_)  -> [l]
  | Stop l           -> [l]
    

(* SIGBOVIK 2014 Markov chain-based sequence generator *)

module Util : sig
  val listFlatten : 'a option list -> 'a list 
end = struct
  let rec listFlatten l =
    match l with
    | []           -> []
    | (Some x)::tl -> x::(listFlatten tl) 
    | None:: tl    -> listFlatten tl
end

module Trie : sig
  type 'a t
  type 'a weight
  type 'a count
  
  val normalize : 'a count -> 'a weight
end = struct
  type 'a t =
   | Leaf
   | Node of ('a * 'a t) list

  type 'a weight = ('a * float) t
  type 'a count = ('a * int) t

  let rec normalize wted = match wted with
    | Leaf -> Leaf
    | Node cts ->
      begin
        let totali = List.fold_left (fun acc curr -> acc + curr) 0 (List.map (fun ((_,ct),_) -> ct) cts) in
        let total = float_of_int totali in
        Node (List.map (fun ((x,ct), cted) -> ((x, float_of_int ct /. total), normalize cted)) cts)
      end
end

module Picker : sig
    val pick : ('a * float) list -> 'a option
end = struct
  let _ = Random.self_init

  let rec pick' l cumwt goalwt =
    match l with
     | []         -> None
     | (x,_)::[]  -> Some(x)
     | (x,wt)::tl ->
         if cumwt <= goalwt && cumwt +. wt > goalwt
         then Some(x)
         else pick' tl (cumwt +. wt) goalwt

  let pick l =
    pick' l 0.0 (Random.float 1.0)    
end

(* let pickPath t = *)
 

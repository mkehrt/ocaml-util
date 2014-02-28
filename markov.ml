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

module Trie = struct
  type 'a t =
   | Leaf
   | Node of ('a * 'a t) list

  type 'a weighted = ('a * float) t
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
 

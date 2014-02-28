type 'a trie =
 | Leaf
 | Node of ('a * 'a trie) list

type 'a weightedTrie = ('a * float) trie

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
 

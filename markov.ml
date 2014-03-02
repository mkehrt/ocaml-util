(* SIGBOVIK 2014 Markov chain-based sequence generator *)

(* I miss Scala so much. *)
module Util : sig
  module List : sig
    val flatten : 'a option list -> 'a list 
  end
  module Option : sig
    val getOrElse : 'a -> 'a option -> 'a
    val map : ('a -> 'b) -> 'a option -> 'b option
  end
end = struct

  module List = struct
    let rec flatten l = match l with
    | []           -> []
    | (Some x)::tl -> x::(flatten tl) 
    | None:: tl    -> flatten tl
  end

  module Option = struct
    let getOrElse x xo = match xo with
      | Some (y) -> y
      | None -> x

     let map f xo = match xo with
       | Some(x) -> Some(f x)
       | None -> None
  end
end

module UL = Util.List
module UO = Util.Option


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

module Trie : sig
  type 'a t
  type 'a weight
  type 'a count
  
  val normalize : 'a count -> 'a weight
  val pickPath : 'a weight -> 'a list
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

  let rec pickPath wted = match wted with
    | Leaf -> []
    | Node l ->
      begin
        let po = Picker.pick (List.map (fun ((x,wt), wted') -> ((x, wted'), wt)) l) in
        UO.getOrElse [] (UO.map (fun (x, wted) -> x :: pickPath wted) po)
      end
end


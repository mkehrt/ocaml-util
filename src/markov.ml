(* SIGBOVIK 2014 Markov chain-based sequence generator *)

(* I miss Scala so much. *)
module Util : sig
  module List : sig
    val flatten : 'a option list -> 'a list 
    val take : int -> 'a list -> 'a list
    val slide : int -> ('b ->'a list -> 'b) -> 'b -> 'a list -> 'b
    module Zipper : sig
      type 'a t
      val unzip : 'a list -> 'a -> 'a t
      val unzipByPredicate : 'a list -> ('a -> bool) -> 'a -> 'a t
      val modify : ('a -> 'a) -> 'a  t -> 'a t
      val map : ('a -> 'b) -> 'a t -> 'b t
      val zip : 'a t -> 'a list
    end
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

   let rec take i l = match i with
     | 0 -> []
     | sn -> (List.hd l)::take (sn-1) l

   let rec slide i f init l =
     if List.length l < i
     then init
     else slide i f (f init (take i l)) (List.tl l)

    module Zipper = struct
      type 'a t = 'a list * 'a * 'a list

      let rec unzipByPredicate l p x =  match l with
        | [] -> (l,x,[])
        | y::tl ->
          if p y
          then ([], y, tl)
          else let (l,z,r) = unzipByPredicate tl p x in (y::l, z, r)

      let rec unzip l x = unzipByPredicate l (fun y -> x = y) x

      let modify f (l,x,r) = (l, f x, r)

      let map f (l, x, r) = (List.map f l, f x, List.map f r)

      let rec zip (l,x,r) = l @ [x] @ r

    end

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

module L = Util.List
module LZ = Util.List.Zipper
module O = Util.Option


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

  val countIterate: 'a count -> 'a list -> 'a count
  val countFromList: 'a list list -> 'a count
  
  val normalize : 'a count -> 'a weight
  val pickPath : 'a weight -> 'a list
end = struct
  type 'a t =
   | Leaf
   | Node of ('a * 'a t) list

  type 'a weight = ('a * float) t
  type 'a count = ('a * int) t

  let rec countIterate (ct: 'a count) l = match (ct, l) with
    | (ct', []) -> ct'
    | (Leaf, x::tl) -> Node [((x, 1), countIterate Leaf tl)]
    | (Node cts, x::tl) ->
      begin
        let zipper = LZ.unzipByPredicate cts (fun ((y,_),_) -> y = x) ((x, 0), Leaf) in
        let zipper' = LZ.modify (fun ((y,i), ct') -> ((y,i+1), countIterate ct' tl)) zipper in
        Node (LZ.zip zipper')
      end

  (* Type inference can't seem to understand the pointsfree version :-( *)
  let countFromList ll = List.fold_left countIterate Leaf ll


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
        O.getOrElse [] (O.map (fun (x, wted) -> x :: pickPath wted) po)
      end
end

module Tokenizer : sig
  type t
  val tokenize : string -> t list
end = struct
  type t = string

  let r = Str.regexp "."
  
  (* Totally not thread safe.  Thanks, Ob^H^HOcaml. *)
  let tokenize s = 
    let rec tokenize' ss start =
      if Str.string_match r s start
      then tokenize' (Str.matched_string s :: ss) (Str.match_end ())
      else ss
    in
    List.rev (tokenize' [] 0)
end

module Run : sig
  val run : unit -> unit
end = struct
  let run () =
    
end

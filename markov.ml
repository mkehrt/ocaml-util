type 'a trie =
 | Leaf
 | Node of ('a * 'a trie) list

type 'a weightedTrie = ('a * float) trie

let _ = Random.self_init
let pick (l : 'a * float list) = ()

(* let pickPath t = *)
 

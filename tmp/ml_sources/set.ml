open List
open Notation

type 'a aset = 'a alist

let set_empty : unit -> 'a aset =
  fun () -> None

let set_add : 'a -> 'a aset -> 'a aset =
  fun x s ->
  if list_mem x s then s
  else Some (x, s)

let set_mem : 'a -> 'a aset -> bool = list_mem

let set_iter : ('a -> unit) -> 'a aset -> unit = list_iter

let set_foldl : ('a -> 'b -> 'a) -> 'a -> 'b aset -> 'a = list_fold

let set_forall : ('a -> bool) -> 'a aset -> bool  = list_forall

let set_cardinal : 'a aset -> int = list_length

let set_subseteq : 'a aset -> 'a aset -> bool =
  fun x y ->  list_forall (fun e ->  set_mem e y) x

let set_equal : 'a aset -> 'a aset -> bool =
  fun x y ->  set_subseteq x y && set_subseteq y x

let f = fun x -> x

type 'a alist = ('a * 'a alist) option

let list_cons =
  fun elem list -> Some (elem, list)

let list_head : 'a alist -> 'a option = fun l ->
  match l with
  | Some a -> Some (fst a)
  | None -> None

let list_int_head : int alist -> int option = fun l ->
  list_head l

let list_tail = fun l ->
  match l with
  | Some a -> snd a
  | None -> None

(* let list_length =
 *   let rec length l =
 *     match l with
 *     | None -> 0
 *     | Some a -> 1 + length (snd a) in
 *   length *)

(* let list_fold =
 *   let rec fold handler acc l =
 *     match l with
 *     | Some a -> let f = fst a in
 *         let s = snd a in
 *         let acc = (handler acc f) in
 *         fold handler acc s
 *     | None -> acc
 *   in fold *)

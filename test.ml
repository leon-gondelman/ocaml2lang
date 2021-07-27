let f = fun x -> x

type 'a alist = ('a * 'a alist) option

let list_cons =
  fun elem list -> Some (elem, list)

(* let list_head : 'a alist -> 'a option = fun l ->
 *   match l with
 *   | Some (a, _) -> Some a
 *   | None -> None *)

(* let list_int_head : int alist -> int option = fun l ->
 *   list_head l *)

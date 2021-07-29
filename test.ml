let f = fun x -> x

type 'a alist = ('a * 'a alist) option

type ('a, 'b) sum_t = InjL of 'a | InjR of 'b

let list_cons =
  fun elem list -> Some (elem, list)

let list_head : 'a alist -> 'a option = fun l ->
  match l with
  | Some a -> Some (fst a)
  | None -> None

let foo = fun x ->
  match x with
  | InjL x -> x
  | InjR x -> x

let bar = fun x ->
  match x with
  | InjL y -> begin match y with
      | InjL z -> z
      | InjR z -> z end
  | InjR y -> y

let bar = fun x ->
  match x with
  | InjR y ->
      begin match y with
      | InjL z -> z
      | InjR z -> z end
  | InjL y -> y

let list_int_head : int alist -> int option = fun l ->
  list_head l

let blabla = fun f g x y ->
  g (f (x y)) (f (x y))

let list_tail = fun l ->
  match l with
  | Some a -> snd a
  | None -> None

let rec length l =
  match l with
  | None -> 0
  | Some a -> 1 + length (snd a)

open List

let foo = fun l -> length l

let rec fold handler acc l =
  match l with
  | Some a -> let f = fst a in
      let s = snd a in
      let acc = (handler acc f) in
        fold handler acc s
  | None -> acc

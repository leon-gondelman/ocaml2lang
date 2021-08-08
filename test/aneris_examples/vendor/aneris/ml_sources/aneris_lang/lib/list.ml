open! Lang

type 'a alist = ('a * 'a alist) option

(* let[@notation
 *   "Notation \"[ ]\" := (list_nil) (format \"[ ]\") : expr_scope."]
 *   list_nil = None *)

let[@notation
  "Notation \"[ ]\" := (list_nil) (format \"[ ]\") : expr_scope."]
  list_nil = None

let[@notation
  "Infix \"::\" := list_cons (at level 60, right associativity) : expr_scope."] list_cons elem list = Some (elem, list)

let list_head l =
  match l with
    Some a -> Some (fst a)
  | None -> None

let list_tail =
 fun l -> match l with
            Some a -> (snd a)
          | None -> None

let rec list_fold =
  fun handler acc l ->
  match l with
    Some a -> let f = fst a in
              let s = snd a in
              let acc = (handler acc f) in
              list_fold handler acc s
  | None -> acc

let rec list_iter =
  fun handler l ->
  match l with
    Some a ->
     let tail = snd a in
     handler (fst a);
     list_iter handler tail
  | None -> ()

let rec list_length =
  fun l ->
  match l with
    Some a -> 1 + list_length (snd a)
  | None -> 0

let rec list_nth =
  fun l i ->
     match l with
       Some a ->
       if i = 0 then Some (fst a)
       else list_nth (snd a) (i - 1)
     | None -> None


let rec list_mem x l =
  match l with
    Some a ->
     let head = fst a in
     let tail = snd a in
     (x = head) || list_mem x tail
  | None -> false

 let rec list_find_remove f l =
  match l with
    Some a ->
     let head = fst a in
     let tail = snd a in
     if f head then Some (head, tail)
     else
       let r = list_find_remove f tail in
       (match r with
          Some b ->
           let head' = fst b in
           let tail' = snd b in
           Some (head', list_cons head tail')
        | None -> None)
  | None -> None

let rec list_sub i l =
  if i <= 0 then None
  else
    match l with
      Some a -> list_cons (fst a) (list_sub (i - 1) (snd a))
    | None -> None

 let rec list_rev_aux l acc =
    match l with
      None -> acc
    | Some p ->
       let h = fst p in
       let t = snd p in
       let acc' = list_cons h acc in
       list_rev_aux t acc'

let list_rev =
  fun l -> list_rev_aux l None

 let rec list_append l r =
  match l with
    None -> r
  | Some p ->
     let h = fst p in
     let t = snd p in
     list_cons h (list_append t r)


let list_is_empty =
  fun l ->
  match l with
    None -> true
  | Some _p -> false


let rec list_forall test l =
  match l with
    None -> true
  | Some p ->
     let h = fst p in
     let t = snd p in
     test h && list_forall test t

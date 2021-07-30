
type 'a alist = NONE | SOME of ('a * 'a alist)

let list_nil = NONE
let list_nilV = NONE

let list_cons =
 fun elem list -> SOME (elem, list)

let list_head =
 fun l -> match l with
            SOME a -> Some (fst a)
          | NONE -> None

let list_tail =
 fun l -> match l with
            SOME a -> (snd a)
          | NONE -> NONE

let rec list_fold =
  fun handler acc l ->
  match l with
    SOME a -> let f = fst a in
              let s = snd a in
              let acc = (handler acc f) in
              list_fold handler acc s
  | NONE -> acc

let rec list_iter =
  fun handler l ->
    match l with
      SOME a ->
       let tail = snd a in
       handler (fst a);
       list_iter handler tail
    | NONE -> ()

let rec list_length =
  fun l ->
  match l with
    SOME a -> 1 + list_length (snd a)
  | NONE -> 0

let rec list_nth =
  fun l i ->
     match l with
       SOME a ->
       if i = 0 then Some (fst a)
       else list_nth (snd a) (i - 1)
     | NONE -> None

let rec list_mem x l =
  match l with
    SOME a ->
     let head = fst a in
     let tail = snd a in
     (x = head) || list_mem x tail
  | NONE -> false

let rec list_find_remove f l =
  match l with
    SOME a ->
     let head = fst a in
     let tail = snd a in
     if f head then SOME (head, tail)
     else
       let r = list_find_remove f tail in
       (match r with
          SOME b ->
           let head' = fst b in
           let tail' = snd b in
           SOME (head', list_cons head tail')
        | NONE -> NONE)
  | NONE -> NONE

let rec list_sub i l =
  if i <= 0 then NONE
  else
    match l with
      SOME a -> list_cons (fst a) (list_sub (i - 1) (snd a))
    | NONE -> NONE

let rec list_rev_aux l acc =
    match l with
      NONE -> acc
    | SOME p ->
       let h = fst p in
       let t = snd p in
       let acc' = list_cons h acc in
       list_rev_aux t acc'

let list_rev =
  fun l -> list_rev_aux l NONE

let rec list_append l r =
  match l with
    NONE -> r
  | SOME p ->
     let h = fst p in
     let t = snd p in
     list_cons h (list_append t r)


let list_is_empty =
  fun l ->
  match l with
    NONE -> true
  | SOME _ -> false


let rec list_forall test l =
  match l with
    NONE -> true
  | SOME p ->
     let h = fst p in
     let t = snd p in
     test h && list_forall test t

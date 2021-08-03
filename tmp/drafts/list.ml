
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

let list_fold =
  let rec fold handler acc l =
  match l with
    SOME a -> let f = fst a in
              let s = snd a in
              let acc = (handler acc f) in
              fold handler acc s
  | NONE -> acc
  in fold


let list_iter =
  let rec iter handler l =
    match l with
      SOME a ->
       let tail = snd a in
       handler (fst a);
       iter handler tail
    | NONE -> ()
  in iter


let list_length =
  let rec length l =
    match l with
      SOME a -> 1 + length (snd a)
    | NONE -> 0
  in length


let list_nth =
  let rec nth l i =
     match l with
       SOME a ->
       if i = 0 then Some (fst a)
       else nth (snd a) (i - 1)
     | NONE -> None
  in nth

let list_mem =
 let rec find x l =
     match l with
       SOME a ->
         let head = fst a in
         let tail = snd a in
         (x = head) || find x tail
     | NONE -> false
 in find


let list_find_remove =
  let rec find_remove f l =
     match l with
       SOME a ->
       let head = fst a in
       let tail = snd a in
       if f head then SOME (head, tail)
       else
         let r = find_remove f tail in
        (match r with
           SOME b ->
           let head' = fst b in
           let tail' = snd b in
           SOME (head', list_cons head tail')
         | NONE -> NONE)
     | NONE -> NONE
  in find_remove

let list_sub =
  let rec sub i l =
    if i <= 0 then NONE
    else
      match l with
        SOME a -> list_cons (fst a) (sub (i - 1) (snd a))
      | NONE -> NONE
  in sub

let list_rev_aux =
  let rec rev_aux l acc =
    match l with
      NONE -> acc
    | SOME p ->
       let h = fst p in
       let t = snd p in
       let acc' = list_cons h acc in
       rev_aux t acc'
  in rev_aux


let list_rev =
  fun l -> list_rev_aux l NONE

let list_append =
  let rec app l r =
    match l with
      NONE -> r
    | SOME p ->
      let h = fst p in
      let t = snd p in
      list_cons h (app t r)
  in app

let list_is_empty =
  fun l ->
  match l with
    NONE -> true
  | SOME _ -> false


let list_forall =
  let rec forall test l =
    match l with
     NONE -> true
    | SOME p ->
       let h = fst p in
       let t = snd p in
       test h && forall test t
in forall

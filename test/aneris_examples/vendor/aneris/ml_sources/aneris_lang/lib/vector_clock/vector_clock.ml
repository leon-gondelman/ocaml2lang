open! Lang
open! Notation
open Network_util
open List

let rec vect_make len init =
  if len = 0 then list_nil
  else list_cons init (vect_make (len - 1) init)

  let vect_nth vec i  =
    unSOME (list_nth vec i)

let rec vect_update vec i v =
  match vec with
    Some a ->
      if i = 0 then list_cons v (list_tail vec)
      else list_cons (fst a) (vect_update (snd a) (i - 1) v)
  | None -> None


let vect_inc vec i =
  let v = (vect_nth vec i) + 1
  in vect_update vec i v

let rec vect_leq v1 v2 =
  match v1 with
    Some a1 ->
      (match v2 with
         Some a2 ->
           (fst a1 <= fst a2) && vect_leq (snd a1) (snd a2)
       | None -> false)
  | None -> list_is_empty v2

let vect_applicable v1 v2 i =
  let rec vect_applicable_aux j v1 v2 =
    match v1 with
      Some a1 ->
        (match v2 with
           Some a2 ->
             (if j = i then
                ((fst a1) = fst a2 + 1)
              else
                (fst a1 <= fst a2))
             && vect_applicable_aux (j + 1) (snd a1) (snd a2)
         | None -> false)
    | None -> list_is_empty v2
  in vect_applicable_aux 0 v1 v2

let rec vect_serialize v =
  match v with
    Some a -> i2s (fst a) ^ "_" ^ vect_serialize (snd a)
  | None -> ""

let rec vect_deserialize s =
  match findFrom s 0 '_' with
    Some i ->
      let x  = unSOME (s2i (substring s 0 i)) in
      let tail =
        let length = strlen s in
        let start = i + 1 in
        vect_deserialize (substring s start (length - start)) in
      list_cons x tail
  | None -> list_nil

(* let f p =
 *   let ((x, y), z) = p
 *   in ((x,y),z) *)

(* let ((x,y),z) as t = ((1,2),3) *)

let f () =
  let (l,r) as t1 = (42,42) in
  let (((x,y),z),w) as t2 = (((1,2),3),4) in
  l + r + x


(* let f x = *)
  (* let ((x,y),(z,w)) as t = x in *)
  (* x *)

let f () =
  let x = 1 in
  let y = 2 in
  x + y

(*
let f p =
  let x = fst (fst p) in
  let y = snd (fst p) in
  let z = snd p in
  ((x,y),z)
*)

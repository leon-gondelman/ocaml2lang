(* let f p =
 *   let ((x, y), z) = p
 *   in ((x,y),z) *)


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

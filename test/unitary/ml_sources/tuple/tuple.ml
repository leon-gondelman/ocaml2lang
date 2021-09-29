open Unsafe


let () = ignore(42); ()


let f () =
  unsafe (fun () ->
      Printf.printf "multi-line";
      Printf.printf "is it working ?";
      Printf.printf "or not ?";
      Printf.printf "must make it work for";
      Printf.printf "test unsafe";)

let fs x = (unsafe (fun () -> Printf.printf "%d!" x))

let g x =
  let (a,b) = x in
  let (c,d) = a in
  let (e,f) = b in
  c + d + e + f

let h x =
  let ((a,b),(c,d)) = x in
    a + b + c + d


(* let f x = *)
  (* let ((x,y),(z,w)) as t = x in *)
  (* x *)

(* let f () =
 *   let x = 1 in
 *   let y = 2 in
 *   x + y *)

(*
let f p =
  let x = fst (fst p) in
  let y = snd (fst p) in
  let z = snd p in
  ((x,y),z)
*)

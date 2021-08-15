(* open! Network
 * open! Lang
 * open! Notation
 * open! Network_util
 * open! List
 * open! Set
 * open! Serialization_type *)

(* let int_ser _v =  "0" *)

(* let int_deser s = unSOME (s2i s) *)

(* let int_serialization =
 *   { dbs_ser   = int_ser;
 *     dbs_deser = int_deser } *)

(* let f (x[@metavar]) (y[@metavar]) = (x, y) *)

(* let f (x[@metavar]) y = (x, y) *)

(* let prod_ser (serA[@metavar]) (serB[@metavar]) : ('a * 'b) -> string =
 *   fun v ->
 *   let s1 = serA (fst v) in
 *   let s2 = serB (snd v) in
 *   (i2s (strlen s1)) ^ "_" ^ s1 ^ s2
 *
 * let prod_deser (deserA[@metavar "base_lang.val"]) (deserB[@metavar "base_lang.val"]) =
 *   fun s ->
 *     match findFrom s 0 '_' with
 *       Some i ->
 *       let len = unSOME (s2i (substring s 0 i)) in
 *       let s1 = substring s (i + 1) len in
 *       let s2 = substring s (i + 1 + len)
 *                              (strlen s - (i + 1 + len)) in
 *       let v1 = deserA s1 in
 *       let v2 = deserB s2 in
 *       (v1, v2)
 *     | None -> assert false
 *
 *
 * let prod_serialization (sA[@metavar "serialization"]) (sB[@metavar "serialization"]) =
 *   { dbs_ser   = prod_ser sA.dbs_ser sB.dbs_ser ;
 *     dbs_deser = prod_deser sA.dbs_deser sB.dbs_deser } *)

(* second metavar will be ignored *)
(* let wrong_a_bit (x[@metavar]) y (z[@metavar]) = (x,(y,z)) *)

(* let int_prod_serialization = prod_serialization int_serialization int_serialization *)

(* let f s = (prod_deser int_deser int_deser) s *)

(*q
let p1 = (1,(2,(3,4)))
let p2 = (((1,2),3),4)

let fp1 = fst p1
let fp2 = snd p2

let r = ref (ref 0)
let l = !(!r)
let f x y = x := (y := 42; !y)
let q x y z =
  x := y ;
  z

let y x = assert x
*)

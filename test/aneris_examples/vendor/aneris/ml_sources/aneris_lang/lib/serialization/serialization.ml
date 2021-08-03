open Lang
open List
open Network_Util

type 'a serialization =
  { dbs_ser : 'a -> string;
    dbs_deser : string -> 'a}

let int_ser v = i2s v

let int_deser v = unSOME (s2i v)

let int_serialization : int serialization =
  { dbs_ser   = int_ser;
    dbs_deser = int_deser }

let unit_ser =
  fun _u -> ""

let unit_deser =
  fun _s -> ()

let unit_serialization : unit serialization =
  { dbs_ser   = unit_ser;
    dbs_deser = unit_deser }

let string_ser =
  fun x -> x

let string_deser =
  fun x -> x

let string_serialization =
  { dbs_ser   = string_ser;
    dbs_deser = string_deser }

let prod_ser (serA[@Gvar]) (serB[@Gvar]) =
  fun v ->
  let s1 = serA (fst v) in
  let s2 = serB (snd v) in
  (i2s (strlen s1)) ^ "_" ^ s1 ^ s2

let prod_deser (deserA[@Gvar]) (deserB[@Gvar]) =
  fun s ->
    match findFrom s 0 '_' with
      Some i ->
      let len = unSOME (s2i (substring s 0 i)) in
      let s1 = substring s (i + 1) len in
      let s2 = substring s (i + 1 + len)
                             (strlen s - (i + 1 + len)) in
      let v1 = deserA s1 in
      let v2 = deserB s2 in
      (v1, v2)
    | None -> assert false

let prod_serialization
      (sA[@Gvar] : 'a serialization) (sB[@Gvar] : 'b serialization)
    : ('a * 'b) serialization =
  { dbs_ser   = prod_ser sA.dbs_ser sB.dbs_ser ;
    dbs_deser = prod_deser sA.dbs_deser sB.dbs_deser }
(* TODO *)
(* let saddr_ser s =
  match s with  SADDR (ip, p) -> prod_ser string_ser int_ser (ip, p)

let saddr_deser s =
  let (ip, p) = prod_deser string_deser int_deser s in
  SADDR (ip, p)

let saddr_serialiazation =
  { dbs_ser   = saddr_ser ;
    dbs_deser = saddr_deser } *)

let sum_ser  (serA[@Gvar]) (serB[@Gvar])  =
    fun v ->
    match v with
      InjL x -> "L" ^ "_" ^ serA x
    | InjR x -> "R" ^ "_" ^ serB x

 let sum_deser (deserA[@Gvar]) (deserB[@Gvar]) =
   fun s ->
   let tag = substring s 0 2 in
   let rest = substring s 2 (strlen s - 2) in
   if tag = "L_" then
     InjL (deserA rest)
   else
     if tag = "R_" then
       InjR (deserB rest)
     else
       assert false;;

 let sum_serialization
       (sA[@Gvar] : 'a serialization)
       (sB[@Gvar] : 'b serialization)
     : ('a, 'b) sumTy) serialization =
   { dbs_ser   = sum_ser sA.dbs_ser sB.dbs_ser ;
     dbs_deser = sum_deser sA.dbs_deser sB.dbs_deser }

 let option_ser (ser[@Gvar])  =
   sum_ser unit_ser ser

 let option_deser (deser[@Gvar])  =
   sum_deser unit_deser deser

 let option_serialization (s[@Gvar])   =
  { dbs_ser   = option_ser s.dbs_ser ;
    dbs_deser = option_deser s.dbs_deser }

 let list_ser (ser[@Gvar])  =
   let rec list_ser v =
     match v with
       SOME a ->
        let hd = ser (fst a) in
        let tl = list_ser (snd a) in
        (i2s (strlen hd)) ^ "_" ^ hd ^ tl
     | NONE -> ""
   in list_ser

 let list_deser (deser[@Gvar]) =
   let rec list_deser s =
     match findFrom s 0 '_' with
       Some i ->
        let len = unSOME (s2i (substring s 0 i)) in
        let h = substring s (i + 1) len in
        let t = substring s (i + 1 + len)
                   (strlen s - (i + 1 + len)) in
        let hd  = deser h in
        let tail = list_deser t in
        list_cons hd tail
     | None -> NONE
   in list_deser

 let list_serialization
       (s[@Gvar] : 'a serialization) : ('a 'alist) serialization =
  { dbs_ser   = list_ser ser.dbs_ser ;
    dbs_deser = list_deser ser.dbs_deser }

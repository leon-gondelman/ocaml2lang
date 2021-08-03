open Network
open Lang
open Notation
open List
open Set

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

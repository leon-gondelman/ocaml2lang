open! Lang
open! Notation

let coin_flip () =
  let l = ref true in
  fork (l := false);
  !l

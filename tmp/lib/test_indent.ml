
(* TODO: remove me after fixing pp *)
let f e1 e2 = e1; (e1; e2; e2)

(* TODO: remove me after fixing pp *)
let g e1 =
  let a =
    let x = e1 (42, 12, 23232232) in x
  in a

(* TODO: remove me after fixing pp *)
let h e1 e2 e3 =
  if
    true
  then
    e1
  else
    e2;
  e3

(* TODO: remove afgter fixing identation *)
let f x =
  let rec g y z = if true then g y z else false in
  g

(* TODO: remove after fixing identation *)
let f x =
  let rec g y z = if true then g y (z - 1) else false in
  g 42

let id x = x

let rec f x =
  let rec g y = y in g

(* TODO: remove after fixing identation *)
let a x =
  let b y =
    let rec c z =
      if z = 0 then id 0 else c (z - 1) in
    c y in
  b x

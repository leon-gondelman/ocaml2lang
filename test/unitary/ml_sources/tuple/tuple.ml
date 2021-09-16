
let f p =
  let x = fst (fst p) in
  let y = snd (fst p) in
  let z = snd p in
  ((x,y),z)

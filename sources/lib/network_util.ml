open Lang
open Network
open List

let unSOME o = match o with
    None -> assert false
  | Some x -> x

let sendto_all socket ns msg =
  list_iter (fun n -> sendTo socket msg n) ns

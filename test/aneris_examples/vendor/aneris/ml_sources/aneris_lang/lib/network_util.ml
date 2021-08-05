open Network
open Lang
open Notation
open List
open Set

let unSOME o = match o with
    None -> assert false
  | Some x -> x

let sendto_all socket ns msg =
  list_iter (fun n -> sendTo socket msg n) ns

let wait_receivefrom =
  fun socket test ->
  let rec loop () =
     let msg = unSOME (receiveFrom socket) in
     if test msg then msg else loop () in
  loop ()

let sendto_all_set =
  fun socket x msg ->
  set_iter (fun n -> let _l = sendTo socket msg n in ()) x

let receivefrom_all =
  fun socket nodes ->
  let rec recv n =
    let msg = unSOME (receiveFrom socket) in
    let sender = snd msg in
    if sender = n then (fst msg)
    else recv n in
  list_fold  (fun acc n -> list_append acc (Some (recv n, None))) None nodes

let wait_receivefrom_all =
  fun socket nodes test ->
  let rec recv n =
    let msg = unSOME (receiveFrom socket) in
    let sender = snd msg in
    let m = fst msg in
    if (sender = n) && (test m) then m
    else recv n in
  list_fold  (fun acc n -> list_append acc (Some (recv n, None))) None nodes

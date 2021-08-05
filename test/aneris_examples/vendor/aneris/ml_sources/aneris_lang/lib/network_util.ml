open Network
open Lang
open Notation
open List
open Set

let unSOME o = match o with
    None -> assert false
  | Some x -> x

let sendto_all skt ns msg =
  list_iter (fun n -> sendTo skt msg n) ns

let rec listen skt handler =
  match receiveFrom skt with
  | Some m ->
     let msg = fst m in
     let sender = snd m in
     handler msg sender
  | None -> listen skt handler

let wait_receivefrom skt test =
  let rec loop () =
     let msg = unSOME (receiveFrom skt) in
     if test msg then msg else loop () in
  loop ()

let sendto_all_set =
  fun skt x msg ->
  set_iter (fun n -> let _l = sendTo skt msg n in ()) x

let receivefrom_all =
  fun skt nodes ->
  let rec recv n =
    let msg = unSOME (receiveFrom skt) in
    let sender = snd msg in
    if sender = n then (fst msg)
    else recv n in
  list_fold  (fun acc n -> list_append acc (Some (recv n, None))) None nodes

let wait_receivefrom_all =
  fun skt nodes test ->
  let rec recv n =
    let msg = unSOME (receiveFrom skt) in
    let sender = snd msg in
    let m = fst msg in
    if (sender = n) && (test m) then m
    else recv n in
  list_fold  (fun acc n -> list_append acc (Some (recv n, None))) None nodes

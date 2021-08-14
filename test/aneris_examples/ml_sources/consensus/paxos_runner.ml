open! Lang
open! Notation
open! List
open! Set
open! Map
open Network
open! Serialization_type
open Serialization
open Paxos_code
open Unix

let list_to_alist l : 'a alist =
  Stdlib.List.fold_left (fun acc x -> list_cons x acc) list_nil l



let ip = (gethostbyname "localhost").h_addr_list.(0)
let a1 = makeAddress (string_of_inet_addr ip) 1081
let a2 = makeAddress (string_of_inet_addr ip) 1082
let a3 = makeAddress (string_of_inet_addr ip) 1083
let l1 = makeAddress (string_of_inet_addr ip) 1084
let l2 = makeAddress (string_of_inet_addr ip) 1085
let p1 = makeAddress (string_of_inet_addr ip) 1086
let p2 = makeAddress (string_of_inet_addr ip) 1087
let c  = makeAddress (string_of_inet_addr ip) 1088
let acceptors = list_to_alist (Stdlib.List.map to_saddr [ a1; a2; a3 ])
let learners  = list_to_alist (Stdlib.List.map to_saddr [ l1; l2 ])

let client_pp valS addr =
  let v = client valS addr in
  Format.printf "\nThe returned value (after assert) is %d.\n" v;
  v

let paxos_runner () =
  if Array.length Sys.argv < 2
  then (prerr_endline "Usage: run <node> <int> \n\
                      \ where <node> is in {a1 a2 a3 l1 l2 p1 p2 c} and\
                      \ if <node>=p* then provide args <int>"; exit 2);
  let _ =
    match Sys.argv.(1) with
    | "a1" -> acceptor int_serializer learners (to_saddr a1)
    | "a2" -> acceptor int_serializer learners (to_saddr a2)
    | "a3" -> acceptor int_serializer learners (to_saddr a3)
    | "l1" -> learner' int_serializer acceptors (to_saddr l1) (to_saddr c)
    | "l2" -> learner' int_serializer acceptors (to_saddr l2) (to_saddr c)
    | "c"  -> client_pp int_serializer (to_saddr c)
    | "p1" ->
        let z1 = Stdlib.int_of_string Sys.argv.(2) in
        proposer' int_serializer acceptors (to_saddr p1) 0 2 z1
    | "p2" ->
        let z2 = Stdlib.int_of_string Sys.argv.(2) in
        proposer' int_serializer acceptors (to_saddr p2) 1 2 z2
    | _    -> assert false
  in ()

let () = Unix.handle_unix_error (paxos_runner) ()

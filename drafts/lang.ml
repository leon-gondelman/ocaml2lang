(* #directory "+threads";;
 #load "unix.cma";;
 #load "threads.cma";; *)

open Unix
open Network

(* type ('a, 'b) sumTy = InjL of 'a | InjR of 'b *)

let[@builtin "MakeAddress"] makeAddress ip port =
  ADDR_INET (inet_addr_of_string ip, port)

type protocol = IPROTO_UDP
let num_of_protocol = function
  | IPROTO_UDP -> 0

let[@builtin "NewSocket"] socket x y z = socket x y (num_of_protocol z)


(* let[@builtin] udp_socket () = socket PF_INET SOCK_DGRAM 0 *)

let[@builtin] receiveFrom skt =
  let buffer = Bytes.create 65536 in
  try
    match recvfrom skt buffer 0 65536 [] with
    | len, (ADDR_INET (_, _) as sockaddr) ->
       let msg = Bytes.sub_string buffer 0 len in
       Some (msg, to_saddr sockaddr)
    | _ -> assert false
  with
    Unix_error (EAGAIN, _,_)
  | Unix_error (EWOULDBLOCK, _, _) -> None

(* translate only name *)
let[@builtin] sendTo skt msg sa =
  sendto skt (Bytes.of_string msg) 0 (String.length msg) [] (of_saddr sa)

(* translate only name *)
let[@builtin] socketBind socket addr = bind socket (of_saddr addr)

exception OnlyPosTimeout

let makeDecimal n =
  let f = float_of_int n in
  if n < 0 then raise OnlyPosTimeout;
  let rec aux q =
    if q < 1. then q
    else aux (q /. 10.)
  in aux f

(* translate only name *)
let[@builtin] setReceiveTimeout sh n m =
  let fn = float_of_int n in
  let fm = makeDecimal m in
  Unix.setsockopt_float sh SO_RCVTIMEO (fn +. fm)

let[@builtin] fork e =
  let _ = Thread.create (fun () -> e) () in ()

let[@builtin] findFrom e0 e1 e2 =
  String.index_from_opt e0 e1 e2

let[@builtin] substring e0 e1 e2 =
  try String.sub e0 e1 e2
  with Invalid_argument _ -> ""

(* (UnOp StringLength e) *)
let[@UnOp "StringLength"] strlen = String.length

(* (UnOp StringOfInt e) *)
let[@UnOp "StringOfInt"] stringOfInt = string_of_int

(* Translate to UnOp IntOfString e *)
let[@UnOp "IntOfString"] intOfString = int_of_string_opt

(* Notations: *)
(* Notation i2s e := (UnOp StringOfInt e)%E (only parsing). *)
(* Notation s2i e := (UnOp IntOfString e)%E (only parsing). *)
(* Notation strlen e := (UnOp StringLength e)%E (only parsing). *)

(* #directory "+threads";;
 #load "unix.cma";;
 #load "threads.cma";; *)

open Unix

type ('a, 'b) sumTy = InjL of 'a | InjR of 'b
 (* type 'a opt = (unit, 'a) sumTy *)

type ip_address = string
type saddr = SADDR of (ip_address * int)

(* internal *)
let to_saddr s =
  match s with
    ADDR_UNIX _ -> assert false
  | ADDR_INET (ip, p) -> SADDR (string_of_inet_addr ip, p)

(* internal *)
let of_saddr s =
  match s with
  | SADDR (ip, p) -> ADDR_INET (inet_addr_of_string ip, p)


(* internal *)
exception OnlyPosTimeout

(* internal *)
let makeDecimal n =
  let f = float_of_int n in
  if n < 0 then raise OnlyPosTimeout;
  let rec aux q =
    if q < 1. then q
    else aux (q /. 10.)
  in aux f

let ip_of_sockaddr s =
  match s with
    ADDR_UNIX _ -> assert false
  | ADDR_INET (ip, _) -> ip

let port_of_sockaddr s =
  match s with
    ADDR_UNIX _ -> assert false
  | ADDR_INET (_, p) -> p


let ip_of_address s =
  match s with
  | SADDR (ip, _) -> ip

let port_of_address s =
  match s with
  | SADDR (_, p) -> p


let unSOME o = match o with
    None -> assert false
  | Some x -> x

(* translate only name *)
let makeAddress [@built-in] ip port = ADDR_INET (inet_addr_of_string ip, port)

(* translate only name *)
let socket () = socket PF_INET SOCK_DGRAM 0

(* translate only name *)
let receiveFrom skt =
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
let sendTo skt msg sa =
  sendto skt (Bytes.of_string msg) 0 (String.length msg) [] (of_saddr sa)

(* translate only name *)
let socketBind socket addr = bind socket (of_saddr addr)

(* translate only name *)
let setReceiveTimeout sh n m =
  let fn = float_of_int n in
  let fm = makeDecimal m in
  Unix.setsockopt_float sh SO_RCVTIMEO (fn +. fm)

(* translate only name *)
let fork e =
  let _ = Thread.create (fun () -> e) () in ()

let findFrom e0 e1 e2 =
  String.index_from_opt e0 e1 e2

let strlen = String.length

let s2i s = int_of_string_opt s

let i2s n = string_of_int n


let substring e0 e1 e2 =
  try String.sub e0 e1 e2
  with Invalid_argument _ -> ""

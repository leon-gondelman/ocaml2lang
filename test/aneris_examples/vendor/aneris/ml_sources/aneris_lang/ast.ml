open Unix

type ('a, 'b) sumTy = InjL of 'a | InjR of 'b

type protocol = IPPROTO_UDP

type saddr = SADDR of (string * int)

let num_of_protocol = function
  | IPPROTO_UDP -> 0

let to_saddr s =
  match s with
    ADDR_UNIX _ -> assert false
  | ADDR_INET (ip, p) -> SADDR (string_of_inet_addr ip, p)

let of_saddr s =
  match s with
  | SADDR (ip, p) -> ADDR_INET (inet_addr_of_string ip, p)


let ip_of_sockaddr s =
  match s with
    ADDR_UNIX _ -> assert false
  | ADDR_INET (ip, _) -> ip

let port_of_sockaddr s =
  match s with
    ADDR_UNIX _ -> assert false
  | ADDR_INET (_, p) -> p

let[@builtin "ip_of_address"] ip_of_address s =
  match s with
  | SADDR (ip, _) -> ip

let[@builtin "port_of_address"] port_of_address s =
  match s with
  | SADDR (_, p) -> p


let[@builtinAtom "MakeAddress"] makeAddress (ip : string) (port : int) =
  ADDR_INET (inet_addr_of_string ip, port)

let[@builtinAtom "NewSocket"] socket x y z = socket x y (num_of_protocol z)

(* let[@builtinAtom] udp_socket () = socket PF_INET SOCK_DGRAM 0 *)

let[@builtinAtom "ReceiveFrom"] receiveFrom skt =
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
let[@builtinAtom "SendTo"] sendTo skt msg sa =
  sendto skt (Bytes.of_string msg) 0 (String.length msg) [] (of_saddr sa)

(* translate only name *)
let[@builtinAtom "SocketBind"] socketBind socket addr = bind socket (of_saddr addr)

exception OnlyPosTimeout

let makeDecimal n =
  let f = float_of_int n in
  if n < 0 then raise OnlyPosTimeout;
  let rec aux q =
    if q < 1. then q
    else aux (q /. 10.)
  in aux f

(* translate only name *)
let[@builtinAtom "SetReceiveTimeout"] setReceiveTimeout sh n m =
  let fn = float_of_int n in
  let fm = makeDecimal m in
  Unix.setsockopt_float sh SO_RCVTIMEO (fn +. fm)

let[@builtinAtom "Fork"] fork e =
  let _ = Thread.create (fun () -> e) () in ()

let[@builtinAtom "FindFrom"] findFrom e0 e1 e2 =
  String.index_from_opt e0 e1 e2

let[@builtinAtom "Substring"] substring e0 e1 e2 =
  try String.sub e0 e1 e2
  with Invalid_argument _ -> ""

(* (UnOp StringLength e) *)
let[@builtinUnOp "StringLength"] strlen = String.length

(* (UnOp StringOfInt e) *)
let[@builtinUnOp "StringOfInt"] stringOfInt = string_of_int

(* Translate to UnOp IntOfString e *)
let[@builtinUnOp "IntOfString"] intOfString = int_of_string_opt

(* Translate to UnOp StringOfInt e *)
let[@builtinAtom "i2s"] i2s = string_of_int

(* Translate to UnOp IntOfString e *)
let[@builtinAtom "s2i"] s2i = int_of_string_opt

let [@builtinAtom "RefLbl"] ref_lbl _s e = ref e


let[@builtinAtom "NewLock"] newlock = fun () -> Mutex.create ()
let[@builtinAtom "TryAcquire"] try_acquire = fun l -> Mutex.try_lock l
let[@builtinAtom "Acquire"] acquire =
  let rec acquire l = if try_acquire l then () else acquire l
  in acquire

let[@builtinAtom "Release"] release = fun l -> Mutex.unlock l


type 'a serializer =
  { s_ser : 'a -> string;
    s_deser : string -> 'a}

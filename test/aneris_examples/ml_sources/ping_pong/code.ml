open Lang
open Notation
open Network_util

let pong addr =
  let skt = socket PF_INET SOCK_DGRAM IPPROTO_UDP in
  socketBind socket addr;
  let rec loop () =
    let m = unSOME (receiveFrom skt) in
    let msg = fst m in
    let sender = snd m in
    (if msg = "PING"
     then sendTo skt "PNG" sender
     else assert false) ;
    loop () in
  loop ()

let ping addr server =
  let skt = socket PF_INET SOCK_DGRAM IPPROTO_UDP in
  socketBind skt addr;
  sendTo skt "PING" server;
  fst (unSOME (receiveFrom skt))

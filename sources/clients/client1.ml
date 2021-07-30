open Lang
open List
open Network_util


let client skt msg addrlist =
  let dst = unSOME (list_nth i addrlist) in
  sendTo skt msg dst

open Unix

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

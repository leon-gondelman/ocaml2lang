open Lang
open! Notation

let tag_of_message msg =
  match findFrom msg 0 '_' with
    Some i  -> substring msg 0 i
  | None -> "UNDEFINED"

let value_of_message msg =
  match findFrom msg 0 '_' with
    Some i  -> let length = strlen msg in
               let start  = i + 1 in
               substring msg start (length - start)
  | None -> "UNDEFINED"

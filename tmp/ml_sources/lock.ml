(* TODO: check if already added to Dune
     (library
     (name aneris_lib)
     (libraries ocaml-compiler-libs.common threads)) *)

let[@builtin] newlock = fun () -> Mutex.create ()
let[@builtin] try_acquire = fun l -> Mutex.try_lock l
let[@builtin] acquire =
  let rec acquire l = if try_acquire l then () else acquire l
  in acquire

let[@builtin] release = fun l -> Mutex.unlock l

(* TODO: check if already added to Dune
     (library
     (name aneris_lib)
     (libraries ocaml-compiler-libs.common threads)) *)

let[@builtin "NewLock"] newlock = fun () -> Mutex.create ()
let[@builtin "TryAcquire"] try_acquire = fun l -> Mutex.try_lock l
let[@builtin "Acquire"] acquire =
  let rec acquire l = if try_acquire l then () else acquire l
  in acquire

let[@builtin "Release"] release = fun l -> Mutex.unlock l

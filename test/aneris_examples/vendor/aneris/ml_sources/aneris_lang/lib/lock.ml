(* TODO: check if already added to Dune
     (library
     (name aneris_lib)
     (libraries ocaml-compiler-libs.common threads)) *)

let[@builtinAtom "NewLock"] newlock = fun () -> Mutex.create ()
let[@builtinAtom "TryAcquire"] try_acquire = fun l -> Mutex.try_lock l
let[@builtinAtom "Acquire"] acquire =
  let rec acquire l = if try_acquire l then () else acquire l
  in acquire

let[@builtinAtom "Release"] release = fun l -> Mutex.unlock l

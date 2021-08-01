open Ocaml2lang
open Translate_aneris
open Format

(* let fname = Sys.argv.(1) *)

open Ast

let deps_queue = Queue.create ()
let src_queue = Queue.create ()

let ml_project =
  let cin = open_in "_OCamlProject" in
  let lb = Lexing.from_channel cin in
  Mlparser.ml_project Mllexer.token lb

let mk_output output fname =
  let root_fname = String.split_on_char '/' fname in
  let concat_fname acc f = Filename.concat acc f in
  match root_fname with
  | _ :: xs -> List.fold_left concat_fname output xs
  | _ -> assert false (* TODO *)

let pp_newline fmt () = fprintf fmt "@\n"
let pp_dot fmt () = fprintf fmt "."

let pp_deps fmt (dep, path, _) =
  let import = ml_project.ml_import in
  let map_import x = Hashtbl.find import x in
  let path = String.split_on_char '/' path in
  let path = match path with
    | x :: xs -> map_import x :: xs
    | _ -> assert false in
  fprintf fmt "From @[%a@] Require Import %s."
    (pp_print_list ~pp_sep:pp_dot pp_print_string) path dep

let pp_program fname prog =
  let _output = ml_project.ml_output in
  let fout_name = (String.uncapitalize_ascii fname) ^ ".v" in
  let cout = open_out fout_name in
  let fout = formatter_of_out_channel cout in
  let decls = prog.prog_body in
  let env = prog.prog_env in
  fprintf fout "@[%a@]@\n@\n@[%a@]"
    (Format.pp_print_list ~pp_sep:pp_newline pp_deps) env
    Pp_aneris.pp_program decls;
  close_out cout

let pp_queue (s, prog) =
  pp_program s prog

let source fname =
  let root = ml_project.ml_root in
  let deps = ml_project.ml_depend in
  let nms = Hashtbl.create 16 in
  let add_dep k () = let fname = Filename.concat root k in
    Hashtbl.add nms fname () in
  Hashtbl.iter add_dep deps;
  let p = program nms fname in
  let fname = Filename.chop_extension fname in
  let not_builtin prog = not prog.prog_builtin in
  Queue.add (fname, p) deps_queue;
  let add_decls (s, _path, prog) =
    if not_builtin prog then Queue.add (s, prog) deps_queue in
  iter_env add_decls p.prog_env;
  Queue.iter pp_queue deps_queue

let () =
  let src = ml_project.ml_source in
  Hashtbl.iter (fun k () -> Queue.add k src_queue) src

let () =
  let root = ml_project.ml_root in
  let process_source s = let fname = Filename.concat root s in
    source fname in
  Queue.iter process_source src_queue

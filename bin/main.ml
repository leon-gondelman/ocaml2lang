open Ocaml2lang
open Translate_aneris
open Format

open Ast

let src_queue = Queue.create ()

let ml_project =
  let cin = open_in "_OCamlProject" in
  let lb = Lexing.from_channel cin in
  Mlparser.ml_project Mllexer.token lb

let mk_output output fname =
  let root_fname = String.split_on_char '/' fname in
  let concat_fname acc f = Filename.concat acc f in
  match root_fname with
  | [x] -> x
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

let perm = 0o700

let create_sub_dirs dirname =
  let dirs = String.split_on_char '/' dirname in
  let fdir acc f = Filename.concat acc f in
  let mkdir f =
    if not (Sys.file_exists f) then Unix.mkdir f perm in
  let acc = ref "" in
  List.iter (fun f -> acc := fdir !acc f; mkdir !acc) dirs

let create_sub_dirs dirname =
  if dirname <> "." then create_sub_dirs dirname

let pp_program fname prog =
  let output = ml_project.ml_output in
  let fname = Filename.chop_extension fname in
  let fout_name = (String.uncapitalize_ascii fname) ^ ".v" in
  let fout_name = mk_output output fout_name in
  let dirname = Filename.dirname fout_name in
  create_sub_dirs dirname;
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
  pp_queue (fname, p)

let () =
  let root = ml_project.ml_root in
  let src = ml_project.ml_source in
  Hashtbl.iter (fun k () -> Queue.add k src_queue) src;
  let process_source s = let fname = Filename.concat root s in
    source fname in
  Queue.iter process_source src_queue

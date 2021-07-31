open Ocaml2lang
open Translate_aneris
open Format

let fname = Sys.argv.(1)

(* let ptree =
 *   let cin = open_in fname in
 *   let lb = Lexing.from_channel cin in
 *   Parser.implementation Lexer.token lb
 *
 * let () =
 *   let info = Translate_aneris.create_info () in
 *   let aneris = structure info ptree in
 *   Pp_aneris.pp_program Format.std_formatter aneris *)

let pp_deps fmt dep =
  fprintf fmt "From ... Require Import %s." dep

let pp_newline fmt () = fprintf fmt "@\n"

open Ast

let pp_program fname prog =
  let fout_name = (String.uncapitalize_ascii fname) ^ ".v" in
  let cout = open_out fout_name in
  let fout = formatter_of_out_channel cout in
  let decls = prog.prog_body in
  let env = prog.prog_env in
  fprintf fout "@[%a@]@\n@[%a@]"
    (Ast.pp_env ~pp_sep:pp_newline ~pp_elts:pp_deps) env
    Pp_aneris.pp_program decls;
  close_out cout

let opt_queue = Queue.create ()

let pp_queue (s, prog) =
  pp_program s prog

let () =
  let p = program fname in
  let fname = Filename.chop_extension fname in
  let not_builtin prog = not prog.prog_builtin in
  let add_decls (s, prog) =
    if not_builtin prog then Queue.add (s, prog) opt_queue in
  Queue.add (fname, p) opt_queue;
  iter_env add_decls p.prog_env;
  Queue.iter pp_queue opt_queue

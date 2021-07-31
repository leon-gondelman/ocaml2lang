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

let pp_program fname env decls =
  let fout_name = (String.uncapitalize_ascii fname) ^ ".v" in
  let cout = open_out fout_name in
  let fout = formatter_of_out_channel cout in
  fprintf fout "@[%a@]@\n@[%a@]"
    (Ast.pp_env ~pp_sep:pp_newline ~pp_elts:pp_deps) env
    Pp_aneris.pp_program decls;
  close_out cout

let queue_files = Queue.create ()

let pp_queue (s, decls, env) =
  pp_program s env decls

open Ast

let () =
  let p = program fname in
  let env = p.prog_env in
  let fname = Filename.chop_extension fname in
  (* let cout = open_out fout_name in
   * let fout = formatter_of_out_channel cout in
   * let env = p.prog_env in *)
  Queue.add (fname, p.prog_body, env) queue_files;
  let add_decls s decls = Queue.add (s, decls, env) queue_files in
  iter_env add_decls p.prog_env;
  Queue.iter pp_queue queue_files
  (* fprintf fout "@[%a@]@\n@[%a@]"
   *   (Ast.pp_env ~pp_sep:pp_newline ~pp_elts:pp_deps) env
   *   Pp_aneris.pp_program p.prog_body;
   * close_out cout *)

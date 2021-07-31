open Ocaml2lang
open Translate_aneris

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
  Format.fprintf fmt "From ... Require Import %s." dep

let pp_newline fmt () = Format.fprintf fmt "@\n"

open Format

let () =
  let p = program fname in
  let fout_name = (Filename.chop_extension fname) ^ ".v" in
  let cout = open_out fout_name in
  let fout = Format.formatter_of_out_channel cout in
  let env = p.prog_env in
  (* Ast.iter_env (fun k _ -> fprintf fout "%s@." k) p.prog_env; *)
  (* Format.fprintf fout "@[%a@]@\n%a"
   *   (Format.pp_print_list ~pp_sep:pp_newline pp_deps) p.prog_env *)
  fprintf fout "@[%a@]@\n@[%a@]"
    (Ast.pp_env ~pp_sep:pp_newline ~pp_elts:pp_deps) env
    Pp_aneris.pp_program p.prog_body;
  close_out cout

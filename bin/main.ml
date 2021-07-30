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

let () =
  let p = program fname in
  Pp_aneris.pp_program Format.std_formatter p

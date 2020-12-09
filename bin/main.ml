open Ocaml2lang

let fname = Sys.argv.(1)

let ptree =
  let cin = open_in fname in
  let lb = Lexing.from_channel cin in
  Parser.implementation Lexer.token lb

let () =
  let aneris = structure ptree in
  List.iter (fun str -> Format.eprintf "%a@." pp_decl str) aneris

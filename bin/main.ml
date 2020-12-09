open Ocaml2lang

let fname = Sys.argv.(1)

let ptree =
  let cin = open_in fname in
  let lb = Lexing.from_channel cin in
  Parser.implementation Lexer.token lb

let () =
  let aneris = Translate.structure ptree in
  Lang.pp_decls Format.std_formatter aneris

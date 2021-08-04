{
  open Lexing
  open Mlparser

  let keywords = Hashtbl.create 16
  let () =
    List.iter (fun (x, y) -> Hashtbl.add keywords x y)
      [
        "OUTPUT_ROOT", OUTPUT;
        "IMPORT_PATH_PREFIX", IMPORT;
        "ML_SOURCES", SOURCES;
        "ML_DEPENDENCIES", DEPENDENCIES;
      ]

  let get_keyword s =
    try Hashtbl.find keywords s with Not_found -> LIDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let newline = '\n'
let space = [' ' '\t' '\r']
let lalpha = ['a'-'z' '_' '.']
let ualpha = ['A'-'Z' '_']
let alpha = lalpha | ualpha
let digit = ['0'-'9']
let lident = lalpha (alpha | digit | '\'' | '/')*
let uident = ualpha (alpha | digit | '\'')*
let uuident = ualpha+
let hexadigit = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let op_char_1 = ['=' '<' '>' '~']
let op_char_2 = ['+' '-']
let op_char_3 = ['*' '/' '\\' '%']
let op_char_4 = ['!' '$' '&' '?' '@' '^' '.' ':' '|' '#']
let op_char_1234 = op_char_1 | op_char_2 | op_char_3 | op_char_4
let op_char_234  = op_char_2 | op_char_3 | op_char_4
let op_char_34   = op_char_3 | op_char_4

rule token = parse
  | newline
      { newline lexbuf; token lexbuf }
  | space+
      { token lexbuf }
  | ":"
      { COMMA }
  | "-V"
      { VENDOR }
  | uuident as id
      { get_keyword id }
  | lident as id
      { LIDENT id }
  | eof
      { EOF }
  | _ as c
      { failwith ("Illegal Character " ^ (Char.escaped c)) }

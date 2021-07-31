%{
    open Ml_project

    type res =
      | ROutput of (string * string) list
      | RImport of (string * string) list
      | RSource of string list
      | RDepend of string list

    let build_ml_project file =
      let ml_project = mk_ml_project () in
      let add_out (s1, s2) = Hashtbl.add ml_project.ml_output s1 s2 in
      let add_outs l = List.iter add_out l in
      let add_imp (s1, s2) = Hashtbl.add ml_project.ml_import s1 s2 in
      let add_imps l = List.iter add_imp l in
      let add_src s = Hashtbl.add ml_project.ml_source s () in
      let add_srcs l = List.iter add_src l in
      let add_dep s = Hashtbl.add ml_project.ml_depend s () in
      let add_deps l = List.iter add_dep l in
      let dispatch = function
        | ROutput l -> add_outs l
        | RImport l -> add_imps l
        | RSource l -> add_srcs l
        | RDepend l -> add_deps l in
      List.iter dispatch file;
      ml_project

%}

%token <string> LIDENT
%token EOF
%token COMMA
%token OUTPUT IMPORT SOURCES DEPENDENCIES

%start <Ml_project.ml_project> ml_project

%%

ml_project:
| file = section* EOF { build_ml_project file }
;

section:
| OUTPUT COMMA map = assoc* { ROutput map }
| IMPORT COMMA map = assoc* { RImport map }
| SOURCES COMMA source = source* { RSource source }
| DEPENDENCIES COMMA source = source* { RDepend source }
;

assoc:
| id1 = LIDENT id2 = LIDENT { (id1, id2) }
;

source:
| id = LIDENT { id }
;

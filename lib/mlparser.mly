%{
    open Ml_project

    type res =
      | ROutput of string * string
      | RImport of (string * string) list
      | RSource of string list
      | RDepend of (string * bool) list

    let mk_output (s1, s2) = ROutput (s1, s2)

    type uc_ml_project = {
        mutable uc_root   : string;
        mutable uc_output : string;
        uc_import : (string, string) Hashtbl.t;
        uc_source : (string, unit) Hashtbl.t;
        uc_depend : (string, bool) Hashtbl.t;
      }

    let mk_uc_ml_project () = {
      uc_root   = "";
      uc_output = "";
      uc_import = Hashtbl.create 16;
      uc_source = Hashtbl.create 16;
      uc_depend = Hashtbl.create 16;
    }

    let close_uc uc = {
      ml_root = uc.uc_root;
      ml_output = uc.uc_output;
      ml_import = uc.uc_import;
      ml_source = uc.uc_source;
      ml_depend = uc.uc_depend;
    }

    let build_ml_project file =
      let uc = mk_uc_ml_project () in
      let add_outs (s1, s2) = uc.uc_root <- s1; uc.uc_output <- s2 in
      let add_imp (s1, s2) = Hashtbl.add uc.uc_import s1 s2 in
      let add_imps l = List.iter add_imp l in
      let add_src s = Hashtbl.add uc.uc_source s () in
      let add_srcs l = List.iter add_src l in
      let add_dep (s, b) = Hashtbl.add uc.uc_depend s b in
      let add_deps l = List.iter add_dep l in
      let dispatch = function
        | ROutput (s1, s2) -> add_outs (s1, s2)
        | RImport l -> add_imps l
        | RSource l -> add_srcs l
        | RDepend l -> add_deps l in
      List.iter dispatch file;
      close_uc uc

%}

%token <string> LIDENT
%token VENDOR
%token EOF
%token COMMA
%token OUTPUT IMPORT SOURCES DEPENDENCIES

%start <Ml_project.ml_project> ml_project

%%

ml_project:
| file = section* EOF { build_ml_project file }
;

section:
| OUTPUT COMMA map = assoc { mk_output map }
| IMPORT COMMA map = assoc* { RImport map }
| SOURCES COMMA source = source* { RSource source }
| DEPENDENCIES COMMA source = vendor_source* { RDepend source }
;

assoc:
| id1 = LIDENT id2 = LIDENT { (id1, id2) }
;

source:
| id = LIDENT { id }
;

vendor_source:
| v = boption(VENDOR) id = LIDENT { (id, v) }

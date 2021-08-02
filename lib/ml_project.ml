type ml_project = {
  ml_root   : string;
  ml_output : string;
  ml_import : (string, string) Hashtbl.t;
  ml_source : (string, unit) Hashtbl.t;
  ml_depend : (string, unit) Hashtbl.t;
}

let mk_ml_project ml_root ml_output = {
  ml_root;
  ml_output;
  ml_import = Hashtbl.create 16;
  ml_source = Hashtbl.create 16;
  ml_depend = Hashtbl.create 16;
}

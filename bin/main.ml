open Ocaml2lang
open Translate_aneris
open Format

open Ast

type bak = Backup | Rewrite

let backup = ref Backup
let gen = ref false
let cleanall = ref false
let src_queue = Queue.create ()

let usage_msg = "ocaml2aneris [options]"

let spec = [ "--backup", Arg.Unit (fun () -> backup := Backup),
             "create .bak files when there exists a .v file";
             "--rewrite", Arg.Unit (fun () -> backup := Rewrite),
             "rewrite existing .v files";
             "--clean", Arg.Unit (fun () -> gen := true),
             "clean generated .v files";
             "--clean-all", Arg.Unit (fun () -> gen := true; cleanall := true),
             "clean generated .v .v.bak files and _generated";]

let usage () = Arg.usage spec usage_msg; exit 1

let set_file _ = () (* TODO: patch in case we decide to have cl files *)

let () = Arg.parse spec set_file usage_msg

let backup = !backup
let clean_gen = !gen
let clean_all = !cleanall
let ml_project =
  let cin = open_in "_OCamlProject" in
  let lb = Lexing.from_channel cin in
  Mlparser.ml_project Mllexer.token lb

let mk_output output fname =
  let root_fname = String.split_on_char '/' fname in
  let concat_fname acc f = Filename.concat acc f in
  match root_fname with
  | [x] -> x
  | _ :: xs -> List.fold_left concat_fname output xs
  | _ -> assert false (* TODO *)

let pp_newline fmt () = fprintf fmt "@\n"
let pp_dot fmt () = fprintf fmt "."

let pp_deps fmt (dep, path, _) =
  let exception Found of string list in
  let rec mk_import real key bind = match real, key with
    | _, [] -> raise (Found (bind :: real))
    | x :: xs, y :: ys when x = y -> mk_import xs ys bind
    | _ -> () in
  let mk_import real key = mk_import real (String.split_on_char '/' key) in
  let import = ml_project.ml_import in
  let path = String.split_on_char '/' path in
  let path = try Hashtbl.iter (mk_import path) import; path
    with Found p -> p in
  fprintf fmt "From @[%a@] Require Import %s."
    (pp_print_list ~pp_sep:pp_dot pp_print_string) path dep

let perm = 0o700

let create_sub_dirs dirname =
  let dirs = String.split_on_char '/' dirname in
  let fdir acc f = Filename.concat acc f in
  let mkdir f = if not (Sys.file_exists f) then Unix.mkdir f perm in
  let acc = ref "" in
  List.iter (fun f -> acc := fdir !acc f; mkdir !acc) dirs

let create_sub_dirs dirname =
  if dirname <> "." then create_sub_dirs dirname

let generated = ref false

let pp_generated fname =
  let fgen = "_generated" in
  let cout_gen =
    if Sys.file_exists fgen && not !generated then open_out fgen
    else open_out_gen [Open_creat; Open_append] 0o666 fgen in
  if  not !generated then Format.eprintf "Created: %s@." fgen;
  generated := true;
  let fout_gen = Format.formatter_of_out_channel cout_gen in
  Format.eprintf "Created: %s@." fname;
  Format.fprintf fout_gen "%s@." fname;
  close_out cout_gen

let pp_program fname prog =
  if Sys.file_exists fname then
    begin match backup with
    | Backup  ->
       let backup = fname ^ ".bak" in
       Format.eprintf "Renamed: %s --> %s@." fname backup;
       Sys.rename fname backup
    | Rewrite -> () end;
  let cout = open_out fname in
  let fout = formatter_of_out_channel cout in
  let decls = prog.prog_body in
  let env = prog.prog_env in
  fprintf fout "@[%a@]@\n@\n@[%a@]"
    (Format.pp_print_list ~pp_sep:pp_newline pp_deps) env
    Pp_aneris.pp_program decls;
  close_out cout;
  pp_generated fname

let pp_program fname prog =
  let output = ml_project.ml_output in
  let fname = Filename.chop_extension fname in
  let fout_name = (String.uncapitalize_ascii fname) ^ ".v" in
  let fout_name = mk_output output fout_name in
  let dirname = Filename.dirname fout_name in
  create_sub_dirs dirname;
  pp_program fout_name prog

let pp_queue (s, prog) =
  pp_program s prog

let source fname =
  let root = ml_project.ml_root in
  let deps = ml_project.ml_depend in
  let nms = Hashtbl.create 16 in
  let add_dep k b =
    let fname = if b then k else Filename.concat root k in
    Hashtbl.add nms fname () in
  Hashtbl.iter add_dep deps;
  let p = program nms fname in
  pp_queue (fname, p)

let () =
  if clean_gen then begin
    let fgen = "_generated" in
    if Sys.file_exists fgen then begin
      let cin = open_in fgen in
      try while true do
            let fname = input_line cin in
            begin
              try
                Sys.remove fname;
                Format.eprintf "Removed: %s@." fname
              with Sys_error _ -> () end;
            begin
              if clean_all
              then
                try
                  let fnamebak = fname ^ ".bak" in
                  Sys.remove fnamebak;
                  Format.eprintf "Removed: %s@." fnamebak
                with Sys_error _ -> () end;
          done
      with End_of_file ->
        close_in cin;
        begin
        if clean_all
        then
          try
           Sys.remove fgen;
           Format.eprintf "Removed: %s@." fgen
          with Sys_error _ -> ()
        end
      end end
  else
    let root = ml_project.ml_root in
    let src = ml_project.ml_source in
    Hashtbl.iter (fun k () -> Queue.add k src_queue) src;
    let process_source s = let fname = Filename.concat root s in
      source fname in
    Queue.iter process_source src_queue

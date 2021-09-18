open Location
open Longident

module P = Parsetree

module Read = struct

  type structure = {
      str_builtin: bool;        (* generate file iff str_builtin = false *)
      str_program: P.structure; (* AST of OCaml program  *)
      str_fname : string;       (* full name of the source file  *)
    }

  let mk_structure ?(str_builtin=false) str_program str_fname =
    { str_builtin; str_program; str_fname}

  type builtin = string list

  (* Collects from _builtin file the names of source files that should
     not be generated. *)
  let builtin =
    let cin = open_in "_builtin" in
    (* FIXME: check if _bultin is present *)
    let l = ref [] in
    try
      while true do
        let line = input_line cin in
        l := line :: !l
      done;
      assert false
    with End_of_file ->
      let acc = List.rev !l in
      let h = Hashtbl.create 16 in
      List.iter (fun s -> Hashtbl.add h s ()) acc; h

  (* Constructs the AST of OCaml source file *)
  let ptree fname =
    let cin = open_in fname in
    let lb = Lexing.from_channel cin in
    let str_program = Parser.implementation Lexer.token lb in
    let str_builtin = Hashtbl.mem builtin fname in
    let res = mk_structure ~str_builtin str_program fname in
    close_in cin;
    res

end

open Ast

module StringSet = Set.Make(String)
module StringSetSet = Set.Make(StringSet)

type info = { (* auxiliary information needed for translation, such as
                 free variables, local variables, paths, etc. *)
    info_lvars   : (ident, unit) Hashtbl.t;
    info_gvars   : (ident, builtin) Hashtbl.t;
    info_rvars   : known_fields ref;
    info_builtin : bool;
    info_known   : (string, builtin) Hashtbl.t;
    info_nmspace : (string, unit) Hashtbl.t;
    info_fname   : string;
    mutable info_deps   : string list;
    mutable info_env    : env;
  }

let create_info info_builtin fname = {
    info_lvars = Hashtbl.create 16;
    info_gvars = Hashtbl.create 16;
    info_rvars = ref (mk_fields ());
    info_builtin;
    info_known = Hashtbl.create 16;
    info_nmspace = Hashtbl.create 16;
    info_fname = fname;
    info_deps    = [];
    info_env     = mk_env ();
  }

let add_known info id builtin =
  Hashtbl.add info.info_known id builtin

let is_builtin info = info.info_builtin

(* let mk_bultin env known fields =
 *   mk_aneris_program env [] known fields *)

(* let return_builtin info l =
 *   if is_builtin info then l else [] *)

let mk_lamb binder expr =
  Rec (BAnon, binder, expr)


let rec decl_name_of_pat info pat = match pat.P.ppat_desc with
  | Ppat_var s -> s.txt
  | Ppat_constraint (p, _) -> decl_name_of_pat info p
  | Ppat_construct ({txt = Lident "()"; _}, _) -> "<>"
  | _ ->
      Format.eprintf
             "\nIn file %s at line %d:\n  this pattern\
              \ is not supported for global declarations.\n"
             info.info_fname pat.ppat_loc.loc_start.pos_lnum;
           exit 1

let rec attrs_of_pat info pat = match pat.P.ppat_desc with
  | Ppat_var _ ->   pat.P.ppat_attributes
  | Ppat_constraint (p, _) -> attrs_of_pat info p
  | _ -> []


(* Extracts ident from OCaml patterns (fun names, formal args, etc) *)
let rec name_of_pat info pat = match pat.P.ppat_desc with
  | Ppat_var s -> s.txt
  | Ppat_constraint (p, _) -> name_of_pat info p
  | Ppat_construct ({txt = Lident "()"; _}, _) -> "<>"
  | Ppat_alias (_, s) -> s.txt
  | _ ->
      Format.eprintf
             "\nIn file %s at line %d:\n  this pattern\
              \ is not supported.\n"
             info.info_fname pat.ppat_loc.loc_start.pos_lnum;
      exit 1

let transform info r pat =
  let rec aux path p = match p.P.ppat_desc with
    | Ppat_var s -> [s.txt, path (Var (Vlvar r))]
    | Ppat_constraint (p, _) -> [name_of_pat info p, path (Var (Vlvar r))]
    | Ppat_construct ({txt = Lident "()"; _}, _) -> ["<>", path (Var (Vlvar r))]
    | Ppat_tuple [p1; p2] ->
        let fl = aux (fun x -> Fst (path x)) p1 in
        let sl = aux (fun x -> Snd (path x)) p2 in
        fl @ sl
    | Ppat_tuple l when List.length l <> 2 ->
        failwith "tuples that are not pairs are not supported"
    | _ -> failwith "pattern not supported (not pair)"
  in aux (fun x -> x) pat

let add_info_lvar info id loc =
    begin match Hashtbl.find_opt info.info_gvars id with
    | None | Some BNone  -> Hashtbl.add info.info_lvars id ()
    | Some (BBuiltin _) | Some (BUnOp _) -> Format.eprintf
             "\nIn file %s at line %d:\n  The keyword %s is reserved. (lvar error)\n"
             info.info_fname loc.loc_start.pos_lnum id;
           exit 1
    end

let remove_info_lvar info id = Hashtbl.remove info.info_lvars id

(* To be completed with all possible builtin translation *)
let node_from_builtin f s args = match s, args with
  | "MakeAddress", [expr1; expr2] ->
      MakeAddress (expr1, expr2)
  | "GetAddrInfo", [expr] ->
      GetAddrInfo expr
  | "NewSocket", [expr1; expr2; expr3] ->
     NewSocket (expr1, expr2, expr3)
  | "SocketBind", [expr1; expr2] ->
     SocketBind (expr1, expr2)
  | "SendTo", [expr1; expr2; expr3] ->
     SendTo (expr1, expr2, expr3)
  | "ReceiveFrom", [expr] ->
     ReceiveFrom expr
  | "SetReceiveTimeout", [expr1; expr2; expr3] ->
     SetReceiveTimeout (expr1, expr2, expr3)
  | "Substring", [expr1; expr2; expr3] ->
     Substring (expr1, expr2, expr3)
  | "FindFrom", [expr1; expr2; expr3] ->
     FindFrom (expr1, expr2, expr3)
  | "Fork", [f; e] ->
     Fork (App (f, e))
  | "RefLbl", [Var (Vlvar expr1); expr2] ->
     Alloc ((Some expr1), expr2)
  | "RefLbl", [Val (LitV LitString s); expr2] ->
     Alloc ((Some s), expr2)
  | "s2i", [expr] ->
     UnOp (IntOfString, expr)
  | "i2s", [expr] ->
     UnOp (StringOfInt, expr)
  | "NewLock", [expr] ->
     ENewLock expr
  | "TryAcquire", [expr] ->
     ETryAcquire expr
  | "Acquire", [expr] ->
     EAcquire expr
  | "Release", [expr] ->
     ERelease expr
  | s, _ ->
      Format.eprintf
      "\nIn file %s \n  the built-in %s is not supported.\n"
     f s;
    exit 1

let node_from_unop s args = match s, args with
  | "StringLength", [expr] ->
     UnOp (StringLength, expr)
  | "StringOfInt", [expr] ->
     UnOp (StringOfInt, expr)
  | "IntOfString", [expr] ->
     UnOp (IntOfString, expr)
  | _ -> assert false (* TODO *)

let find_file_deps info f =
  let exception Found of string * string in
  try
    let check_file k () = let fname = Filename.concat k f in
                          if Sys.file_exists fname then raise (Found (f, k)) in
    Hashtbl.iter check_file info.info_nmspace;
    Format.eprintf
      "\nIn file %s \n  dependency %s not found.\n"
      info.info_fname f;
    exit 1
  with Found (s, path) -> s, path

let add_assert info =
  let nms = info.info_nmspace in
  let exception Found of string in
  try Hashtbl.iter (fun k () -> let fname = Filename.concat k "assert.v" in
                                if Sys.file_exists fname then (raise (Found k))) nms
  with Found s -> Format.eprintf "Found assert in %s@." s

(* Currently not useful in Coq *)
let rec normalize_expr e0 = match e0 with
    | ENone -> Val NoneV
    | InjL (Val v) -> Val (InjLV v)
    | InjR (Val v) -> Val (InjRV v)
    | ESome (Val v) -> Val (SomeV v)
    | Pair (Val v1, Val v2) -> Val (PairV (v1, v2))
    | InjL e ->
       begin
         match normalize_expr e with
         | Val v -> Val (InjLV v)
         | en -> InjL en
       end
    | InjR e ->
       begin
         match normalize_expr e with
         | Val v -> Val (InjRV v)
         | en -> InjR en
       end
    | ESome e ->
       begin match normalize_expr e with
       | Val v -> Val (SomeV v)
       | en -> ESome en
       end
    | Pair (e1, e2) ->
       begin match normalize_expr e1, normalize_expr e2 with
       | Val v1, Val v2 -> Val (PairV (v1, v2))
       | en1, en2 -> Pair (en1, en2)
       end
    | Rec (b1, b2, e) ->
       Rec (b1, b2, normalize_expr e)
    | App (e1, e2) ->
       App (normalize_expr e1, normalize_expr e2)
    | UnOp (uop, e1) ->
       UnOp (uop, normalize_expr e1)
    | BinOp (bop, e1, e2) ->
       BinOp (bop, normalize_expr e1, normalize_expr e2)
    | If (e1, e2, e3) ->
       If (normalize_expr e1, normalize_expr e2, normalize_expr e3)
    | FindFrom (e1, e2, e3) ->
       FindFrom (normalize_expr e1, normalize_expr e2, normalize_expr e3)
    | Substring (e1, e2, e3) ->
       Substring (normalize_expr e1, normalize_expr e2, normalize_expr e3)
    | Fst e -> Fst (normalize_expr e)
    | Snd e -> Snd (normalize_expr e)
    | Case (e, (s1, e1), (s2, e2)) ->
       Case (normalize_expr e, (s1, normalize_expr e1), (s2, normalize_expr e2))
    | Fork e ->
       Fork (normalize_expr e)
    | Alloc (so, e) ->
       Alloc (so, normalize_expr e)
    | Load e ->
       Load (normalize_expr e)
    | Store (e1, e2)  ->
       Store (normalize_expr e1, normalize_expr e2)
    | MakeAddress (e1, e2)  ->
        MakeAddress (normalize_expr e1, normalize_expr e2)
    | GetAddrInfo e ->
        GetAddrInfo (normalize_expr e)
    | NewSocket (e1, e2, e3) ->
       NewSocket (normalize_expr e1, normalize_expr e2, normalize_expr e3)
    | SocketBind  (e1, e2) ->
       SocketBind (normalize_expr e1, normalize_expr e2)
    | SendTo (e1, e2, e3) ->
       SendTo (normalize_expr e1, normalize_expr e2, normalize_expr e3)
    | ReceiveFrom e ->
       ReceiveFrom (normalize_expr e)
    | Eassert e ->
       Eassert (normalize_expr e)
    | ENewLock e ->
       ENewLock (normalize_expr e)
    | ETryAcquire e ->
       ETryAcquire (normalize_expr e)
    | EAcquire e ->
       EAcquire (normalize_expr e)
    | ERelease e ->
       ERelease (normalize_expr e)
    | SetReceiveTimeout (e1, e2, e3) ->
       SetReceiveTimeout
         (normalize_expr e1, normalize_expr e2, normalize_expr e3)
    | ERecord iel ->
       ERecord (List.map (fun (id, e) -> (id, normalize_expr e)) iel)
    | Var _ | Val _ | CAS _  | Start _ | EField _ -> e0

let pp_comma ppf () = Format.fprintf ppf ", "
let pp_list_list fmt ll =
  let pp_list fmt l =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string) l in
  Format.fprintf fmt "{%a}"
    (Format.pp_print_list ~pp_sep:pp_comma pp_list) ll

let value_binding_bultin info P.{pvb_pat; pvb_attributes; _} =
  let is_builtin P.{attr_name = {txt; _}; _} =
    txt = "builtinAtom" || txt = "builtinUnOp"  in
  (* todo: add notations in builtin files *)
  let get_payload payload = match payload with
    | P.PStr
      [{ pstr_desc =
           Pstr_eval
             ({ pexp_desc =
                  Pexp_constant (Pconst_string (spec,_,_)); _ }, _);
         _ };] -> spec
    | _ -> assert false in
  let get_builtin P.{attr_name = {txt; _}; attr_payload; _} = match txt with
    | "builtinAtom" -> BBuiltin (get_payload attr_payload)
    | "builtinUnOp"    -> BUnOp (get_payload attr_payload)
    | _         -> BNone in
  begin try
      let attr = List.find is_builtin pvb_attributes in
      let builtin = get_builtin attr in
      let id = decl_name_of_pat info pvb_pat in
      add_known info id builtin;
    with Not_found -> () end;
  []

let is_metavar info P.{attr_name = {txt; _}; attr_loc; _} =
  if txt = "metavar" then true
  else
    begin
      Format.eprintf
        "\nIn file %s at line %d:\n  Attribute '%s' is not supported.\n"
        info.info_fname attr_loc.loc_start.pos_lnum txt;
      exit 1
    end

let coqparam_argty (attrs : P.attribute list) =
  if List.length attrs <> 1 then assert false
  else
    match (List.hd attrs).attr_payload with
  | P.PStr
      [{ pstr_desc =
           Pstr_eval
             ({ pexp_desc =
                  Pexp_constant (Pconst_string (s,_,_)); _ }, _);
         _ };] ->
      if s = "val" then Some TyVal
      else if s = "serializer" then Some TySerializer
      else failwith "forbidden metavar type"
  | P.PStr [] -> None
  | _ -> assert false

let rec sanity_check_params info expr =
  match expr.P.pexp_desc with
  | Pexp_fun (Nolabel, None, pat, body) ->
     let attrs = attrs_of_pat info pat in
     if not (List.exists (is_metavar info) attrs)
     then sanity_check_params info body
     else
       begin
         Format.eprintf
           "\nIn file %s at line %d:\n meta-vars are not allowed after local vars.\n"
           info.info_fname pat.ppat_loc.loc_start.pos_lnum;
         exit 1
       end
  | _ -> ()

let rec split_coqparams info (acc : (ident * argty option) list) expr =
    match expr.P.pexp_desc with
  | Pexp_fun (Nolabel, None, pat, body) ->
     let pname = decl_name_of_pat info pat in
     let attrs = attrs_of_pat info pat in
      if List.exists (is_metavar info) attrs
     then
       let tyopt = coqparam_argty attrs in
       split_coqparams info ((pname, tyopt) :: acc) body
     else
       begin sanity_check_params info expr; (List.rev acc, expr) end
  | _ ->
     begin sanity_check_params info expr; (List.rev acc, expr) end


(* Extracts ident from OCaml patterns (fun names, formal args, etc) *)
let is_aliased_tuple_of_pat pat = match pat.P.ppat_desc with
  | Ppat_alias ({ ppat_desc = Ppat_tuple _; _} as p, s)  -> Some (s.txt, p)
  | _ -> None

let rec inline_ptuple_as_expr info path_list (body : P.expression) =
  let rec aux fexpr l = match l with
    | [] ->
        fexpr body
    | (var, path) :: tl ->
        let fexpr' e = App (mk_lamb (BNamed var) (fexpr e), path) in
        aux fexpr' tl
  in aux (fun e -> expression info e) (List.rev path_list)

and letin_notbuiltin
          ~isrec
          (info : info)
          ({pvb_pat; pvb_expr; pvb_loc; _ }: P.value_binding)
          (e2 : P.expression) =
  match is_aliased_tuple_of_pat pvb_pat with
  | None ->
      if isrec
      then
        begin
          let fun_name = name_of_pat info pvb_pat in
          add_info_lvar info fun_name pvb_loc;
          let body = expression info pvb_expr in
          let expr2 = expression info e2 in
          remove_info_lvar info fun_name;
          let arg, body = match body with
            | Rec (_, b, e) -> b, e
            | _ -> assert false in
          match expr2 with
          | Var (Vlvar v)
            when v = fun_name -> Rec (BNamed fun_name, arg, body)
          | _ -> App (Rec (BNamed fun_name, arg, body), expr2)
        end
      else
        begin
          let id = name_of_pat info pvb_pat in
          let e1 = expression info pvb_expr in
          add_info_lvar info id pvb_loc;
          let e2 = expression info e2 in
          remove_info_lvar info id;
          App (mk_lamb (BNamed id) e2, e1)
        end
  | Some (r, p) ->
      let pl = transform info r p in
      let e1 = expression info pvb_expr in
      add_info_lvar info r pvb_loc;
      List.iter (fun (var, _) -> add_info_lvar info var pvb_loc)  pl;
      let e2 = inline_ptuple_as_expr info pl e2 in
      List.iter (fun (var, _) -> remove_info_lvar info var)  pl;
      remove_info_lvar info r;
      App (mk_lamb (BNamed r) e2, e1)

and value_binding_notbuiltin
          ~isrec
          (info : info)
          ({pvb_pat; pvb_expr; pvb_loc; _ }: P.value_binding) =
  let add_info_gvar id b =
    try
      begin
        match Hashtbl.find info.info_known id with
        | BNone -> Hashtbl.add info.info_gvars id b
        | _ ->
           Format.eprintf
             "\nIn file %s at line %d:\n\
             \  The ident %s is a reserved keyword. (gvar error)\n"
             info.info_fname pvb_loc.loc_start.pos_lnum id; exit 1
      end
    with Not_found -> Hashtbl.add info.info_gvars id b in
  let remove_info_gvar (id, _) = Hashtbl.remove info.info_gvars id in
  let id = decl_name_of_pat info pvb_pat in
  let (mvars, body) = split_coqparams info [] pvb_expr in
  List.iter (fun (id, _) -> add_info_gvar id BNone) mvars;
  let expr =
    if isrec
    then
      begin
        add_info_lvar info id pvb_loc;
        let expr = expression info body in
        remove_info_lvar info id;
        expr
      end
    else expression info body in
  List.iter remove_info_gvar mvars;
  (id, mvars, expr)

and string_of_longident = function
  | Lapply _ -> assert false (* TODO *)
  | Lident s -> s
  | Ldot (id, s) -> (string_of_longident id) ^ "_" ^ s

and longident info t l = match t with
  | Lapply _ -> assert false (* TODO *)
  | Lident s ->
     if Hashtbl.mem info.info_lvars s then Vlvar s
     else if Hashtbl.mem info.info_gvars s then Vgvar (Gvar s)
     else begin
         Format.eprintf
           ("\nIn file %s at line %d:\n  symbol: '%s' is undefined. (longident error)")
           info.info_fname l.loc_start.pos_lnum s;
         exit 1
       end
  | Ldot (t, s) ->
     (* TODO: open external modules *)
     let v = longident info t l in
     match v with
     | Vgvar x -> Vgvar (Gdot (x, s))
     | Vlvar _ -> assert false

and expression info expr =
  let is_fst P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "fst"; _} -> true
    | _ -> false in
  let is_snd P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "snd"; _} -> true
    | _ -> false in
  let is_plus P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "+"; _} -> true
    | _ -> false in
  let is_minus P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "-"; _} -> true
    | _ -> false in
  let is_mult P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "*"; _} -> true
    | _ -> false in
  let is_quot P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "/"; _} -> true
    | _ -> false in
  let is_mod P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "mod"; _} -> true
    | _ -> false in
  let is_and P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "&&"; _} -> true
    | _ -> false in
  let is_or P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "||"; _} -> true
    | _ -> false in
  let is_xor P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "<>"; _} -> true
    | _ -> false in
  let is_equal P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "="; _} -> true
    | _ -> false in
  let is_leq P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "<="; _} -> true
    | _ -> false in
  let is_lt P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "<"; _} -> true
    | _ -> false in
  let is_string_app P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "^"; _} -> true
    | _ -> false in
  let is_uminus P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "~-"; _} -> true
    | _ -> false in
  let is_not P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "not"; _} -> true
    | _ -> false in
  let is_ref P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "ref"; _} -> true
    | _ -> false in
  let is_assign P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident ":="; _} -> true
    | _ -> false in
  let is_load P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "!"; _} -> true
    | _ -> false in
  let is_ignore P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "ignore"; _} -> true
    | _ -> false in
  let mk_app e1 args =
    let find_builtin id = Hashtbl.find info.info_gvars id in
    let mk_app acc e = App (acc, e) in
    let expr1 = expression info e1 in
    let (_, args) = List.split args in
    let exprl = List.map (expression info) args in
    match expr1 with
    | Var (Vgvar (Gvar id)) ->
       begin match find_builtin id with (* this should not raise Not_found *)
       | BNone -> List.fold_left mk_app expr1 exprl
       | BBuiltin s -> node_from_builtin info.info_fname s exprl
       | BUnOp s -> node_from_unop s exprl end
    | _ -> List.fold_left mk_app expr1 exprl in
  match expr.P.pexp_desc with
  | Pexp_constant c -> Val (LitV (constant c))
  | Pexp_construct (c,o) -> construct info (c,o)
  | Pexp_ident t -> Var (longident info t.txt t.loc)
  | Pexp_fun (Nolabel, None, pat, expr) ->
      begin
        match is_aliased_tuple_of_pat pat with
        | None ->
            begin
              let id = name_of_pat info pat in
              match id with
              | "<>" -> mk_lamb BAnon (expression info expr)
              | _ ->
                  add_info_lvar info id pat.ppat_loc;
                  let e = expression info expr in
                  remove_info_lvar info id;
                  mk_lamb (BNamed id) e
            end
        | Some (r, p) ->
            let pl = transform info r p in
            add_info_lvar info r pat.ppat_loc;
            List.iter (fun (var, _) -> add_info_lvar info var pat.ppat_loc) pl;
            let e = inline_ptuple_as_expr info pl expr in
            List.iter (fun (var, _) -> remove_info_lvar info var) pl;
            remove_info_lvar info r;
            mk_lamb (BNamed r) e
      end
  | Pexp_fun _ ->
     assert false (* TODO *)
  | Pexp_apply (f, [(_, e)]) when is_ignore f ->
     expression info e
  | Pexp_apply (f, [(_, e)]) when is_fst f ->
     Fst (expression info e)
  | Pexp_apply (f, [(_, e)]) when is_snd f ->
     Snd (expression info e)
  | Pexp_apply (f, [(_, e)]) when is_ref f ->
     Alloc (None, (expression info e))
  | Pexp_apply (f, [(_, e)]) when is_load f ->
     Load (expression info e)
  | Pexp_apply (f, [(_, e1); (_, e2)]) when is_assign f ->
     let expr1 = expression info e1 in
     let expr2 = expression info e2 in
     Store (expr1, expr2)
  | Pexp_apply (f, [(_, e1); (_, e2)]) when is_plus f ->
     let expr1 = expression info e1 in
     let expr2 = expression info e2 in
     BinOp (PlusOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_minus f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (MinusOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_mult f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (MultOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_quot f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (QuotOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_mod f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (RemOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_and f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (AndOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_or f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (OrOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_xor f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (XorOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_equal f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (EqOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_leq f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (LeOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_lt f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (LtOp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1); (_, expr2)]) when is_string_app f ->
     let expr1 = expression info expr1 in
     let expr2 = expression info expr2 in
     BinOp (StringApp, expr1, expr2)
  | Pexp_apply (f, [(_, expr1)]) when is_uminus f ->
     let expr1 = expression info expr1 in
     UnOp (MinusUnOp, expr1)
  | Pexp_apply (f, [(_, expr1)]) when is_not f ->
     let expr1 = expression info expr1 in
     UnOp (NegOp, expr1)
  | Pexp_apply (e1, el) ->
     mk_app e1 el
  | Pexp_tuple [expr1 ; expr2] ->
     let e1 = expression info expr1 in
     let e2 = expression info expr2 in
     begin
       match (e1, e2) with
       | Val v1, Val v2 -> Val (PairV (v1, v2))
       | _              -> Pair (e1, e2)
     end
  | Pexp_tuple (_x :: _y :: _) ->
     assert false
  (* let mk_tuple acc e = Pair (acc, e) in (* TODO: check for values *)
     let fst = expression info x in
     let snd = List.map (expression info) xs in
     List.fold_left mk_tuple fst snd *)
  | Pexp_tuple [] ->
     assert false (* TODO *)
  | Pexp_match (e, [c1; c2]) ->
     let expr = expression info e in
     Case (expr, pattern info c1, pattern info c2)
  | Pexp_match _ ->
     assert false (* TODO *)
  | Pexp_constraint (e, _) ->
     expression info e
  | Pexp_ifthenelse (e1, e2, Some e3) ->
     let expr1 = expression info e1 in
     let expr2 = expression info e2 in
     let expr3 = expression info e3 in
     If (expr1, expr2, expr3)
  | Pexp_ifthenelse (e1, e2, None) ->
     let expr1 = expression info e1 in
     let expr2 = expression info e2 in
     If (expr1, expr2, Val (LitV LitUnit))
  | Pexp_let (Nonrecursive, [val_bind], e2) ->
     letin_notbuiltin ~isrec:false info val_bind e2
  | Pexp_let (Recursive, [val_bind], e2) ->
      letin_notbuiltin ~isrec:true info val_bind e2
  | Pexp_sequence (e1, e2) ->
      let expr1 = expression info e1 in
     let expr2 = expression info e2 in
     App (mk_lamb BAnon expr2, expr1)
  | Pexp_assert e ->
     add_assert info;
     Eassert (expression info e)
  | Pexp_record (iel, _) ->
     let field_name (i,_) =
       match i.txt with
       | Lident s -> s
       | _ -> assert false in
     if mem_fields (List.map field_name iel) !(info.info_rvars)
     then
       let mk_field_def (i,e) = (field_name (i,e), expression info e) in
       ERecord (List.map mk_field_def iel)
     else
       begin
         Format.eprintf
           "\nIn file %s at line %d:\n  Some field names are unknown. (record value)\n"
           info.info_fname expr.pexp_loc.loc_start.pos_lnum ;
         exit 1
       end
  | Pexp_open _ ->  assert false (* TODO *)
  | Pexp_field (e, lid) -> record_field info e lid
  | _ -> assert false (* TODO *)

and record_field info expr lid =
  match expr.P.pexp_desc, lid.txt with
  | Pexp_ident {txt = (Lident r); _}, Lident f ->
     EField (Var (Vgvar (Gvar r)), f)
  | _, Lident f ->
     EField (expression info expr, f)
  | _ -> assert false (* only gvars are supported *)


and pattern info P.{pc_lhs; pc_rhs; _} =
  let is_unit P.{ppat_desc; _} = match ppat_desc with
    | Ppat_construct ({txt = Lident "()"; _}, None) -> true
    | _ -> false in
  let get_var_of_pat P.{ppat_desc; _} = match ppat_desc with
    | Ppat_var {txt; _} -> txt
    | _ -> assert false in
  (* let add_info_lvar id = Hashtbl.add info.info_lvars id () in *)
  let add_info_lvar id loc =
    begin match Hashtbl.find_opt info.info_gvars id with
    | None -> Hashtbl.add info.info_lvars id ()
    | _ -> Format.eprintf
             "\nIn file %s at line %d:\n  The keyword %s is reserved.\n"
             info.info_fname loc.loc_start.pos_lnum id;
           exit 1
    end in
  let pat_desc P.{ppat_desc; ppat_loc; _} = match ppat_desc with
    | P.Ppat_any -> assert false (* TODO *)
    | Ppat_var _ ->
       assert false (* not autorized as a top-level pattern *)
    | Ppat_construct ({txt = Lident "None"; _}, None) ->
       "None", BAnon
    | Ppat_construct ({txt = Lident "InjL"; _}, Some p) when is_unit p ->
       "InjL", BAnon
    | Ppat_construct ({txt = Lident "InjL"; _}, Some p) ->
       let v = get_var_of_pat p in
       add_info_lvar v p.ppat_loc;
       "InjL", BNamed v
    | Ppat_construct ({txt = Lident "InjR"; _}, Some p) ->
       let v = get_var_of_pat p in
       add_info_lvar v p.ppat_loc;
       "InjR", BNamed v
    | Ppat_construct ({txt = Lident p; _},
                      Some ({ppat_desc = Ppat_var s; _} as pat)) ->
       add_info_lvar s.txt pat.ppat_loc;
       p, BNamed s.txt
    | _ ->
       Format.eprintf
         "\nIn file %s at line %d:\n\
          \  this pattern construction not supported.\n"
         info.info_fname ppat_loc.loc_start.pos_lnum;
       exit 1 in
  let txt, binder = pat_desc pc_lhs in
  let pc_rhs = expression info pc_rhs in
  txt, mk_lamb binder pc_rhs

and constant = function
    Pconst_integer (t, _) -> LitInt (int_of_string t)
  | Pconst_string (s,_,_) -> LitString s
  | Pconst_char c -> LitString (Char.escaped c)
  | Pconst_float _ -> assert false (* not implemented in AnerisLang *)

and construct info = function
  | ({txt = Lident "()"; loc = _}, None) -> Val (LitV LitUnit)
  | ({txt = Lident "true"; loc = _}, None) -> Val (LitV (LitBool true))
  | ({txt = Lident "false"; loc = _}, None) -> Val (LitV (LitBool false))
  | ({txt = Lident "None"; loc = _}, None) -> ENone
  | ({txt = Lident "Some"; loc = _}, Some expr) ->
     ESome (expression info expr)
     (* begin match e with Val v -> Val (SomeV v) | _ -> ESome e end *)
  | ({txt = Lident "InjL"; _}, Some expr) ->
     InjL (expression info expr)
     (* let e = expression info expr in
      * begin match e with Val v -> Val (InjLV v) | _ -> InjL e end *)
  | ({txt = Lident "InjR"; _}, Some expr) ->
     InjR (expression info expr)
     (* let e = expression info expr in
      * begin match e with Val v -> Val (InjRV v) | _ -> InjR e end *)
  | ({txt = Lident "PF_INET"; _}, None) ->
     Val (LitV (LitAddressFamily PF_INET))
  | ({txt = Lident "SOCK_DGRAM"; _}, None) ->
     Val (LitV (LitSocketType SOCK_DGRAM))
  | ({txt = Lident "IPPROTO_UDP"; _}, None) ->
     Val (LitV (LitProtocol IPPROTO_UDP))
  | ({txt = Lident s; loc = loc},_) ->
     Format.eprintf
       "\nIn file %s at line %d:\n  The value '%s' is currently not supported.\n"
       info.info_fname loc.loc_start.pos_lnum s;
     exit 1
  | _ -> assert false (*TODO : socket address, socket handle? *)


let rec structure info str =
  let body = List.flatten (List.map (structure_item info) str) in
  let env = List.rev info.info_env in
  mk_aneris_program env body info.info_known info.info_rvars info.info_builtin

and structure_item info str_item =
  let add_info_gvar id b = Hashtbl.add info.info_gvars id b in
  let add_known_builtin id b = Hashtbl.add info.info_known id b in
  match str_item.P.pstr_desc with
    (* let binding *)
  | Pstr_value (Nonrecursive, [val_bind]) ->
     if is_builtin info then
       value_binding_bultin info val_bind
     else
       let (id, mvars, expr) =
         value_binding_notbuiltin ~isrec:false info val_bind in
       add_info_gvar id BNone;
       add_known_builtin id BNone;
       [PDecl (id, mvars, expr)]
  (* recursive let binding with no mutual recursion *)
  | Pstr_value (Recursive, [val_bind]) ->
     if is_builtin info then
       value_binding_bultin info val_bind
     else
        let (id, mvars, expr) =
          value_binding_notbuiltin ~isrec:true info val_bind in
        add_info_gvar id BNone;
        add_known_builtin id BNone;
        let arg, body = match expr with
          | Rec (_, b, e) -> b, e
          | _ -> assert false in
        [PDecl (id, mvars, Rec (BNamed id, arg, body))]
  | Pstr_open {popen_expr = {pmod_desc = Pmod_ident m; _}; _} ->
     let fname = string_of_longident m.txt in
     if not (is_builtin info) then begin
         let nms = info.info_nmspace in
         let fname_ml = (String.uncapitalize_ascii fname) ^ ".ml" in
         let fname_ml, path = find_file_deps info fname_ml in
         let fname_ml = Filename.concat path fname_ml in
         let ({prog_known; prog_fields; _} as p) = program nms fname_ml in
         (* add all known symbols to the gvars tables *)
         let add_info_gvar id b = add_info_gvar id b in
         Hashtbl.iter add_info_gvar prog_known;
         info.info_rvars := join_fields !(prog_fields) !(info.info_rvars);
         let fname = String.uncapitalize_ascii fname in
         info.info_env <- add_env info.info_env fname path p
       end;
     (* else ...
              what should we do about [open] inside builtins? *)
     info.info_deps <- fname :: info.info_deps;
     []

  (* exceptions are currently not supported *)
  | Pstr_exception te ->
     if is_builtin info
     then []
     else begin
         Format.eprintf
           "\nIn file %s at line %d:\n  exceptions are not supported."
           info.info_fname te.ptyexn_loc.loc_start.pos_lnum;
         exit 1
       end
  | Pstr_type (_, {ptype_kind; _} :: _) ->
     let get_fieldname _info ({pld_name; _}: P.label_declaration) =
       pld_name.txt in
     if is_builtin info
     then
       begin
         match ptype_kind with
         | Ptype_record l ->
            let flds = List.map (get_fieldname info) l in
            begin
              try
                info.info_rvars := add_fields flds !(info.info_rvars);
                []
              with FieldsAlreadyExist ->
                Format.eprintf
                  "\nIn file %s:\n  some of record fields {%a}\
                   \ are reserved and cannot be redefined.\n"
                  info.info_fname
                  (Format.pp_print_list ~pp_sep:pp_comma
                     Format.pp_print_string) flds;
                exit 1
            end
         | _ -> []
       end
     else
       begin
         match ptype_kind with
         | Ptype_record l ->
            let flds = List.map (get_fieldname info) l in
            begin
              if mem_fields flds !(info.info_rvars)
              then
                (Format.eprintf
                  "\nIn file %s:\n  some of record fields {%a}\
                   \ are reserved and cannot be redefined.\n"
                  info.info_fname
                  (Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string) flds;
                 exit 1)
              else []
            end
         | _ -> []
       end
  | Pstr_attribute a ->
      structure_attribute info a
  | _ -> assert false (* TODO *)

and structure_attribute info a =
  let get_payload = match a.attr_payload with
    | PStr [s] ->
        begin match s.pstr_desc with
          | Pstr_eval
              ({pexp_desc =
                  Pexp_constant (Pconst_string (s, _, _)); _}, _)
            -> s
          | _ -> (Format.eprintf
                    "\nIn file %s:\n attribute payload type of the attribute\
                    \ %s is not supported.\n" info.info_fname a.attr_name.txt;
                  exit 1)
        end
    | PStr [] -> (Format.eprintf
                    "\nIn file %s at line %d:\n attribute with empty payload\
                     \ attribute %s is not supported.\n"
                    info.info_fname
                    a.attr_name.loc.loc_start.pos_lnum a.attr_name.txt;
            exit 1)
    | _ -> (Format.eprintf
                    "\nIn file %s at line %d:\n attribute payload type of the \
                     \ attribute %s is not supported.\n"
                    info.info_fname
                    a.attr_name.loc.loc_start.pos_lnum a.attr_name.txt;
            exit 1) in
  match a.attr_name.txt with
  | "NOTATION" -> [PNotation get_payload]
  | "COMMENT" -> [PComment get_payload]
  | "ocaml.text" -> [PDocComment get_payload]
  | _ -> assert false

  and program nms fname =
  let open Read in
  let {str_builtin; str_program; str_fname} = ptree fname in
  let info = create_info str_builtin str_fname in
  Hashtbl.iter (fun k () -> Hashtbl.add info.info_nmspace k ()) nms;
  structure info str_program

let ptree_of_string s =
  let lb = Lexing.from_string s in
  Parser.implementation Lexer.token lb

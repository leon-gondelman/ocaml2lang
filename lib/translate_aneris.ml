open Location
open Longident

module P = Parsetree

module Read = struct

  type structure = {
    str_builtin: bool;
    str_program: P.structure;
    str_fname : string;
  }

  let mk_structure ?(str_builtin=false) str_program str_fname =
    { str_builtin; str_program; str_fname}

  type builtin = string list

  let builtin =
    let cin = open_in "_builtin" in (* FIXME: check if _bultin is present *)
    let l = ref [] in
    try
      while true do
        let line = input_line cin in
        l := line :: !l
      done;
      assert false
    with End_of_file -> let acc = List.rev !l in
      let h = Hashtbl.create 16 in
      List.iter (fun s -> Hashtbl.add h s ()) acc;
      h

  let ptree fname =
    let cin = open_in fname in
    let lb = Lexing.from_channel cin in
    let str_program = Parser.implementation Lexer.token lb in
    let str_builtin = Hashtbl.mem builtin fname in
    mk_structure ~str_builtin str_program fname

end

open Ast

type info = { (* auxiliary information needed for translation, such as
                 free variables, local variables, paths, etc. *)
  info_lvars   : (ident, unit) Hashtbl.t;
  info_gvars   : (ident, builtin) Hashtbl.t;
  info_builtin : bool;
  info_known   : (string, builtin) Hashtbl.t;
  info_nmspace : (string, unit) Hashtbl.t;
  info_fname   : string;
  mutable info_deps   : string list;
  mutable info_env    : env;
  (* TODO: dependencies, in particular for [assert] *)
}

let create_info info_builtin fname = {
  info_lvars = Hashtbl.create 16;
  info_gvars = Hashtbl.create 16;
  info_builtin;
  info_known = Hashtbl.create 16;
  info_nmspace = Hashtbl.create 16;
  info_fname = fname;
  info_deps    = [];
  info_env     = mk_env ();
}

let add_known info id builtin =
  Hashtbl.add info.info_known id builtin

let mk_lamb binder expr =
  Rec (BAnon, binder, expr)

let rec name_of_pat pat = match pat.P.ppat_desc with
  | Ppat_any -> assert false
  | Ppat_var s -> s.txt
  | Ppat_constraint (p, _) -> name_of_pat p
  | Ppat_construct ({txt = Lident "()"; _}, _) -> "<>"
  | _ -> assert false (* TODO *)

let is_builtin info = info.info_builtin

let mk_bultin env known =
  mk_aneris_program env [] known

let return_builtin info l =
  if is_builtin info then l else []

let is_val = function
  | Val _ -> true
  | _ -> false

let value_binding_bultin info P.{pvb_pat; pvb_attributes; _} =
  let is_builtin P.{attr_name = {txt; _}; _} =
    txt = "builtin" || txt = "UnOp" in
  let get_payload payload = match payload with
    | P.PStr
        [{ pstr_desc =
             Pstr_eval
               ({ pexp_desc =
                    Pexp_constant (Pconst_string (spec, _, _)); _ }, _);
           _ };] -> spec
    | _ -> assert false in
  let get_builtin P.{attr_name = {txt; _}; attr_payload; _} = match txt with
    | "builtin" -> BBuiltin (get_payload attr_payload)
    | "UnOp"    -> BUnOp (get_payload attr_payload)
    | _         -> BNone in
  begin try
    let attr = List.find is_builtin pvb_attributes in
    let builtin = get_builtin attr in
    let id = name_of_pat pvb_pat in
    add_known info id builtin;
  with Not_found -> () end;
  []

(* To be completed with all possible builtin translation *)
let node_from_builtin s args = match s, args with
  | "MakeAddress", [expr1; expr2] ->
      MakeAddress (expr1, expr2)
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
  | "Fork", [expr] ->
      Fork expr
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
  | _ -> assert false

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

let rec structure info str =
  let body = List.flatten (List.map (structure_item info) str) in
  let env = List.rev info.info_env in
  mk_aneris_program env body info.info_known info.info_builtin

and structure_item info str_item =
  let add_info id b = Hashtbl.add info.info_gvars id b in
  let add_known id b = Hashtbl.add info.info_known id b in
  match str_item.P.pstr_desc with
  | Pstr_value (Nonrecursive, [val_bind]) ->
      if is_builtin info then
        value_binding_bultin info val_bind
      else
        let id, expr = value_binding info val_bind  in
        add_info id BNone;
        add_known id BNone;
        [(id, expr)]
  | Pstr_value (Recursive, [val_bind]) ->
      if is_builtin info then
        value_binding_bultin info val_bind
      else
        let id, expr = value_binding info val_bind  in
        let arg, body = match expr with
          | Rec (_, b, e) -> b, e
          | _ -> assert false in
        add_info id BNone;
        add_known id BNone;
        [(id, Rec (BNamed id, arg, body))]
  | Pstr_type _ ->
      []
  | Pstr_open {popen_expr = {pmod_desc = Pmod_ident m; _}; _} ->
      let fname = string_of_longident m.txt in
      if not (is_builtin info) then begin
        let nms = info.info_nmspace in
        let fname_ml = (String.uncapitalize_ascii fname) ^ ".ml" in
        let fname_ml, path = find_file_deps info fname_ml in
        let fname_ml = Filename.concat path fname_ml in
        let ({prog_known; _} as p) = program nms fname_ml in
        (* add all known symbols to the gvars tables *)
        let add_info id b = add_info id b in
        Hashtbl.iter add_info prog_known;
        let fname = String.uncapitalize_ascii fname in
        info.info_env <- add_env info.info_env fname path p
      end;
      (* else ...
              what should we do about [open] inside builtins? *)
      info.info_deps <- fname :: info.info_deps;
      []
  | Pstr_exception te ->
      if is_builtin info then []
      else begin
          Format.eprintf
             "\nIn file %s at line %d:\n  exceptions are not supported."
             info.info_fname te.ptyexn_loc.loc_start.pos_lnum;
          exit 1
        end
  | _ -> assert false (* TODO *)

and value_binding info {pvb_pat; pvb_expr; pvb_loc; _ } =
  (* let add_info_lvar id = Hashtbl.add info.info_lvars id () in *)
  let add_info_lvar id =
    begin match Hashtbl.find_opt info.info_gvars id with
    | None -> Hashtbl.add info.info_lvars id ()
    | _ ->
       Format.eprintf
         "\nIn file %s at line %d:\n  The keyword %s is reserved.\n"
         info.info_fname pvb_loc.loc_start.pos_lnum id;
       exit 1
    end in
  let remove_info_lvar id = Hashtbl.remove info.info_lvars id in
  let id = name_of_pat pvb_pat in
  add_info_lvar id;
  let expr = expression info pvb_expr in
  remove_info_lvar id;
  id, expr

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
            ("\nIn file %s at line %d:\n  symbol: '%s' is undefined.")
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
  (* let add_info_lvar id = Hashtbl.add info.info_lvars id () in *)
  let add_info_lvar id loc =
    begin match Hashtbl.find_opt info.info_gvars id with
    | None -> Hashtbl.add info.info_lvars id ()
    | _ -> Format.eprintf
             "\nIn file %s at line %d:\n  The keyword %s is reserved.\n"
             info.info_fname loc.loc_start.pos_lnum id;
          exit 1
    end in
  (* let add_local_args args = List.iter add_info args in *)
  let remove_info_lvar id = Hashtbl.remove info.info_lvars id in
  (* let remove_local_args args = List.iter remove_info args in *)
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
        | BBuiltin s -> node_from_builtin s exprl
        | BUnOp s -> node_from_unop s exprl end
    | _ -> List.fold_left mk_app expr1 exprl in
  match expr.P.pexp_desc with
  | Pexp_constant c -> Val (LitV (constant c))
  | Pexp_construct (c,o) -> construct info (c,o)
  | Pexp_ident t -> Var (longident info t.txt t.loc)
  | Pexp_fun (Nolabel, None, pat, expr) ->
     let id = name_of_pat pat in
     begin
       match id with
       | "<>" ->
          let expr = expression info expr in
          Rec (BAnon, BAnon, expr)
       | _ ->
          add_info_lvar id pat.ppat_loc;
          let expr = expression info expr in
          remove_info_lvar id;
          Rec (BAnon, BNamed id, expr)
     end
  | Pexp_fun _ ->
      assert false (* TODO *)
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
  | Pexp_ifthenelse (_, _, None) ->
     assert false
  | Pexp_let (Nonrecursive, [val_bind], e2) ->
      let id, expr = value_binding info val_bind in
      add_info_lvar id val_bind.pvb_pat.ppat_loc;
      let expr2 = expression info e2 in
      remove_info_lvar id;
      App (mk_lamb (BNamed id) expr2, expr)
  | Pexp_let (Recursive, [{pvb_pat; _} as val_bind], e2) ->
      let fun_name = name_of_pat pvb_pat in
      add_info_lvar fun_name pvb_pat.ppat_loc;
      let _id, expr = value_binding info val_bind in
      let expr2 = expression info e2 in
      remove_info_lvar fun_name;
      begin
        let arg, body = match expr with
          | Rec (_, b, e) -> b, e
          | _ -> assert false in
        match expr2 with
        | Var (Vlvar v) when v = fun_name ->
           Rec (BNamed fun_name, arg, body)
        | _ ->
           App (Rec (BNamed fun_name, arg, body), expr2)
      end
  | Pexp_sequence (e1, e2) ->
     let expr1 = expression info e1 in
     let expr2 = expression info e2 in
     App (mk_lamb BAnon expr2, expr1)
  | Pexp_assert e ->
      add_assert info;
      Eassert (expression info e)
  | Pexp_open _ ->
      assert false (* TODO *)
  | _ -> assert false (* TODO *)

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
  let pat_desc P.{ppat_desc; _} = match ppat_desc with
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
    | _ -> assert false (* TODO *) in
  let txt, binder = pat_desc pc_lhs in
  let pc_rhs = expression info pc_rhs in
  txt, mk_lamb binder pc_rhs

and constant = function
    Pconst_integer (t, _) -> LitInt (int_of_string t)
  | Pconst_string (s, _, _) -> LitString s
  | Pconst_char c -> LitString (Char.escaped c)
  | Pconst_float _ -> assert false (* not implemented in AnerisLang *)

and construct info = function
  | ({txt = Lident "()"; loc = _}, None) -> Val (LitV LitUnit)
  | ({txt = Lident "true"; loc = _}, None) -> Val (LitV (LitBool true))
  | ({txt = Lident "false"; loc = _}, None) -> Val (LitV (LitBool false))
  | ({txt = Lident "None"; loc = _}, None) -> ENone
  | ({txt = Lident "Some"; loc = _}, Some expr) ->
      let e = expression info expr in
      begin match e with Val v -> Val (SomeV v) | _ -> ESome e end
  (* | ({txt = Lident "::"; loc = _}, Some e) ->
   *     begin match e.pexp_desc with
   *       | Pexp_tuple [e1;e2] ->
   *           Eapp (mk_gvar "::", [expression info   e1;
   *                                expression info   e2])
   *       | _ -> assert false
   *     end *)
  (* | ({txt = Lident "[]"; loc = _}, None) ->
   *     Evalue NONEV *)
  | ({txt = Lident "InjL"; _}, Some expr) ->
      let e = expression info expr in
      begin match e with Val v -> Val (InjLV v) | _ -> InjL e end
  | ({txt = Lident "InjR"; _}, Some expr) ->
      let e = expression info expr in
      begin match e with Val v -> Val (InjRV v) | _ -> InjR e end
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

and program nms fname =
  let open Read in
  let {str_builtin; str_program; str_fname} = ptree fname in
  let info = create_info str_builtin str_fname in
  Hashtbl.iter (fun k () -> Hashtbl.add info.info_nmspace k ()) nms;
  structure info str_program

let ptree_of_string s =
  let lb = Lexing.from_string s in
  Parser.implementation Lexer.token lb

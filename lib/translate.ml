open Location
open Longident

module P = Parsetree

open Lang

let mk_gvar s = Evar (Vgvar (Gvar s))

let rec name_of_pat pat = match pat.P.ppat_desc with
  | Ppat_any -> "_"
  | Ppat_var s -> s.txt
  | Ppat_constraint (p, _) -> name_of_pat p
  | _ -> assert false (* TODO *)

let rec structure str =
  List.flatten (List.map structure_item str)

and structure_item str_item = match str_item.P.pstr_desc with
  | Pstr_value (rec_flag, [val_bind]) ->
      ignore (rec_flag); (* TODO *)
      let id, expr = value_binding val_bind in
      [DDefinition (id, TyVal, expr)]
  | Pstr_type _ ->
      []
  | _ -> assert false (* TODO *)

and value_binding {pvb_pat; pvb_expr; _} =
  name_of_pat pvb_pat, expression [] pvb_expr

and longident params = function
  | Lapply _ -> assert false (* TODO *)
  | Lident s ->
      if List.mem s params then Vlvar s else Vgvar (Gvar s)
  | Ldot (t, s) -> let v = longident [] t in
      match v with
      | Vgvar x -> Vgvar (Gdot (x, s))
      | Vlvar _ -> assert false

and expression params expr =
  let is_fst P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "fst"; _} -> true
    | _ -> false in
  let is_snd P.{pexp_desc; _} = match pexp_desc with
    | Pexp_ident {txt = Lident "snd"; _} -> true
    | _ -> false in
  match expr.pexp_desc with
  | Pexp_constant c -> Evalue (LitV (constant c))
  | Pexp_construct (c,o) -> construct params (c,o)
  | Pexp_ident t -> Evar (longident params t.txt)
  | Pexp_fun (Nolabel, None, pat, expr) ->
      let id = name_of_pat pat in
      let expr = expression (id :: params) expr in
      begin
        match expr with
        | Efun (il, e) -> Efun (id :: il, e)
        | _ -> Efun ([id], expr)
      end
  | Pexp_apply (f, [(_, e)]) when is_fst f ->
      EFst (expression params e)
  | Pexp_apply (f, [(_, e)]) when is_snd f ->
      ESnd (expression params e)
  | Pexp_apply (e1, el) ->
      let expr1 = expression params e1 in
      let (_, args) = List.split el in
      let exprl = List.map (expression params) args in
      Eapp (expr1, exprl)
  | Pexp_tuple expr_list when List.length expr_list = 2 ->
      Etuple (List.map (expression params) expr_list)
  | Pexp_tuple _ ->
      assert false (* TODO *)
  | Pexp_match (e, [c1; c2]) ->
      let expr = expression params e in
      Ecase (expr, pattern params c1, pattern params c2)
  | Pexp_match _ ->
      assert false (* TODO *)
  | Pexp_constraint (e, _) ->
      expression params e
  | Pexp_let (Recursive, _, _) ->
      assert false (* TODO *)
  | _ -> assert false (* TODO *)

and pattern params P.{pc_lhs; pc_rhs; _} =
  let rec pat_desc P.{ppat_desc; _} = match ppat_desc with
    | P.Ppat_any        -> Ppat_any
    | Ppat_var {txt; _} -> Ppat_var txt
    | Ppat_construct ({txt; _}, p) ->
        Ppat_apply (longident params txt, Option.map pat_desc p)
    | _ -> assert false (* TODO *) in
  let pc_lhs = pat_desc pc_lhs in
  let pc_rhs = expression params pc_rhs in
  { pc_lhs; pc_rhs }

and constant = function
    Pconst_integer (t, _) -> LitInt (int_of_string t)
  | Pconst_string (s, _, _) -> LitString s
  | Pconst_char _ ->  assert false (* not implemented in AnerisLang *)
  | Pconst_float _ -> assert false (* not implemented in AnerisLang *)

and construct params = function
  |  ({txt = Lident "()"; loc = _}, None) -> Evalue (LitV LitUnit)
  |  ({txt = Lident "true"; loc = _}, None) -> Evalue (LitV (LitBool true))
  |  ({txt = Lident "false"; loc = _}, None) -> Evalue (LitV (LitBool false))
  |  ({txt = Lident "None"; loc = _}, None) -> Evalue NONEV
  |  ({txt = Lident "Some"; loc = _}, Some expr) ->
      let e = expression params expr in
      begin
        match e with
        | Evalue v -> Evalue (SOMEV v)
        | _ -> Esome e
      end
  | ({txt = Lident "::"; loc = _}, Some e) ->
      begin match e.pexp_desc with
        | Pexp_tuple [e1;e2] ->
            Eapp (mk_gvar "::", [expression params e1; expression params e2])
        | _ -> assert false
      end
  | ({txt = Lident "[]"; loc = _}, None) ->
      Evalue NONEV
  | _ -> assert false (*TODO*)


(* type un_op =
 *   | NegOp | MinusUnOp | StringOfInt | IntOfString | StringLength
 *
 * type bin_op =
 *   | PlusOp | MinusOp | MultOp | QuotOp | RemOp (\* Arithmetic *\)
 *   | AndOp | OrOp | XorOp (\* Bitwise *\)
 *   | ShiftLOp | ShiftROp (\* Shifts *\)
 *   | LeOp | LtOp | EqOp (\* Relations *\)
 *   | StringApp *)

let ptree_of_string s =
  let lb = Lexing.from_string s in
  Parser.implementation Lexer.token lb

let%expect_test _ =
  Lang.pp_decls Format.std_formatter
    (structure (ptree_of_string "let f x = x"));
  [%expect {| Definition f : base_lang.val := λ: "x", "x". |}]

let%expect_test _ =
  Lang.pp_decls Format.std_formatter
    (structure (ptree_of_string "let f x = y"));
  [%expect {| Definition f : base_lang.val := λ: "x", y. |}]

let%expect_test _ =
  Lang.pp_decls Format.std_formatter
    (structure (ptree_of_string "let x = [1;2;3]"));
  [%expect{|
    Definition x : base_lang.val :=
      list_cons #1 (list_cons #2 (list_cons #3 NONEV)). |}]

let%expect_test _ =
  Lang.pp_decls Format.std_formatter
    (structure (ptree_of_string "let f x = List.fold_left (fun x y -> x y z) [1;2;3]"));
  [%expect{|
    Definition f : base_lang.val :=
      λ: "x",
        list_fold (λ: "x" "y", "x" "y" z)
          (list_cons #1 (list_cons #2 (list_cons #3 NONEV))). |}]

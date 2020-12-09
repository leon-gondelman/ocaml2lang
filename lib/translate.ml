open Parsetree
open Location
open Longident

open Lang

let mk_gvar s = Evar (Vgvar (Gvar s))

let name_of_pat pat = match pat.ppat_desc with
  | Ppat_any -> "_"
  | Ppat_var s -> s.txt
  | _ -> assert false (* TODO *)

let rec structure str =
  List.map structure_item str

and structure_item str_item = match str_item.pstr_desc with
  | Pstr_value (rec_flag, [val_bind]) ->
     ignore (rec_flag);
     let id, expr = value_binding val_bind in
     DDefinition (id, TyVal, expr)
  | _ -> assert false (* TODO *)

and value_binding {pvb_pat; pvb_expr; _} =
  name_of_pat pvb_pat, expression [] pvb_expr

and longident params = function
  | Lapply _ -> assert false (* TODO *)
  | Lident s ->
     if List.mem s params then Vlvar s else Vgvar (Gvar s)
  | Ldot (t, s) ->
     let v = longident [] t in
     match v with
       | Vgvar x -> Vgvar (Gdot (x, s))
       | Vlvar _ -> assert false

and expression params expr = match expr.pexp_desc with
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
  | Pexp_apply (e1, el) ->
     let expr1 = expression params e1 in
     let (_, args) = List.split el in
     let exprl = List.map (expression params) args in
     Eapp (expr1, exprl)
  | _ -> assert false (* TODO *)

and constant = function
    Pconst_integer (t, _) -> LitInt (int_of_string t)
  | Pconst_string (s, _) -> LitString s
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

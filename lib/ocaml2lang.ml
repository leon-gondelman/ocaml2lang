type ident = string

type gvar =
  | Gvar of ident
  | Gdot of gvar * ident

type var = Vlvar of ident | Vgvar of gvar

type base_lit =
  | LitInt of int
  | LitBool of bool
  | LitUnit
  | LitString of string
  (* | LitAddressFamily of address_family
   * | LitSocketType of socket_type
   * | LitProtocol of  protocol
   * | LitSocket of socket_handle
   * | LitSocketAddress of socket_address *)

type value =
  | LitV of base_lit
  | PairV of value * value
  | InjLV of value
  | InjRV of value
  | NONEV
  | SOMEV of value

type expr =
  | Evar of var
  | Evalue of value
  | Efun of (ident list) * expr
  | Eapp of expr * (expr list)
  | Esome of expr

  (* | Elet of ident * expr * expr *)

let mk_gvar s = Evar (Vgvar (Gvar s))

let gvartbl = Hashtbl.create 53
let _ =
  List.iter (fun (kwd, tok) -> Hashtbl.add gvartbl kwd tok)
    [ "::", "list_cons";
      "List.hd", "list_head";
      "List.tl", "list_tail";
      "List.fold_left", "list_fold";
      "List.iter", "list_iter";
      "List.length", "list_length";
      "List.nth", "list_nth"]

let rec str_of_gvar = function
  | Gvar  s -> s
  | Gdot (g, s) -> (str_of_gvar g) ^ "." ^ s

let is_complex_value = function
  | LitV _ | PairV _ | NONEV  -> false
  | InjLV _ | InjRV _  | SOMEV _  -> true


(* TODO: make this function more elaborate *)
let complex_syntax = function
  | Evar _ | Evalue _ -> false
  | _ -> true

type typ = TyVal

type decl =
  | DDefinition of ident * typ * expr

(** Pretty printer *)
open Format

let pp_typ fmt = function
  | TyVal -> fprintf fmt "base_lang.val"

let pp_lvar fmt s = fprintf fmt "\"%s\"" s

let rec pp_gvar fmt = function
  | Gvar s -> fprintf fmt "%s" s
  | Gdot (q, s) -> fprintf fmt "%a.%s" pp_gvar q s


let pp_param fmt s = fprintf fmt "\"%s\"" s

let pp_space ppf () = Format.fprintf ppf "@ "

let pp_var fmt = function
  | Vlvar s -> fprintf fmt "%a" pp_lvar s
  | Vgvar v -> fprintf fmt "%a" pp_gvar v


(** Printing of values *)

let pp_litv fmt = function
  | LitInt n -> fprintf fmt "#%d" n
  | LitBool true -> fprintf fmt "#true"
  | LitBool false -> fprintf fmt "#false"
  | LitUnit -> fprintf fmt "#()"
  | LitString s -> fprintf fmt "#\"%s\"" s

let rec pp_value fmt = function
  | LitV bl -> fprintf fmt "%a" pp_litv bl
  | PairV (v1, v2) -> fprintf fmt "(%a, %a)" pp_value v1 pp_value v2
  | NONEV ->  fprintf fmt "NONEV"
  | SOMEV v ->
     if is_complex_value v
     then fprintf fmt "SOMEV (%a)" pp_value v
     else fprintf fmt "SOMEV %a" pp_value v
  | InjLV _ -> assert false (*TODO*)
  | InjRV _ -> assert false (*TODO*)

(** Printing of expressions *)

let rec pp_expr fmt = function
  | Evalue v ->  fprintf fmt "%a" pp_value v
  | Evar v -> fprintf fmt "%a" pp_var v
  | Efun (idl, e) ->
     if complex_syntax e then
       fprintf fmt "@[<hov 2>λ: %a,@\n%a@]"
         (pp_print_list ~pp_sep:pp_space pp_param) idl
         pp_expr e
     else
       fprintf fmt "@[λ: %a, %a@]"
         (pp_print_list ~pp_sep:pp_space pp_param) idl
         pp_expr e
  | Eapp (e1, el) -> pp_app fmt e1 el
  | _ -> assert false (*todo*)

and pp_app fmt e el =
  match (e, el) with
  | (Evar (Vgvar Gvar s), [e1;e2]) when s = "+" || s = "-" || s = "*" ->
     fprintf fmt "(%a %s %a)" pp_expr_pr e1 s pp_expr_pr e2
  | (Evar (Vgvar v), el) ->
     begin
       try
         let str = Hashtbl.find gvartbl (str_of_gvar v) in
         fprintf fmt "%s %a" str (pp_print_list ~pp_sep:pp_space pp_expr_app) el
       with Not_found ->
         fprintf fmt "%a %a" pp_gvar v
           (pp_print_list ~pp_sep:pp_space pp_expr_app) el
     end
  | _ -> fprintf fmt "%a %a" pp_expr_app e
           (pp_print_list ~pp_sep:pp_space pp_expr_app) el

and pp_expr_pr fmt e =
  if complex_syntax e then fprintf fmt "(%a)" pp_expr e
  else  fprintf fmt "%a" pp_expr e

and pp_expr_app fmt = function
  | Evar (Vlvar s) -> fprintf fmt "%a" pp_lvar s
  | Evar (Vgvar v) -> fprintf fmt "%a" pp_gvar v
  | Evalue (LitV bl) ->  fprintf fmt "%a" pp_litv bl
  | _ as expr -> fprintf fmt "%a" pp_expr_pr expr

let pp_decl fmt = function
  | DDefinition (id, typ, expr) ->
     fprintf fmt "Definition %s : %a := %a." id pp_typ typ pp_expr expr



open Parsetree
open Location
open Longident

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

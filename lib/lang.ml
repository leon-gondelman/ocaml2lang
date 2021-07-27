type ident = string

type gvar =
  | Gvar of ident
  | Gdot of gvar * ident

type var =
  (* Local name bound by a heap-lang binder *)
  | Vlvar of ident
  (* Global name bound at the meta-level (e.g. as a Coq definition) *)
  | Vgvar of gvar

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
  | Evar   of var
  | Evalue of value
  | Etuple of expr list
  | Efun   of (ident list) * expr
  | Erec   of ident * ident * expr
  | EFst   of expr
  | ESnd   of expr
  | Eapp   of expr * (expr list)
  | Esome  of expr
  | Ecase  of expr * case * case

and case = {
  pc_lhs: pattern;
  pc_rhs: expr;
}

and pattern =
  | Ppat_any
  | Ppat_var   of string
  | Ppat_apply of var * pattern option

  (* | Elet of ident * expr * expr *)

type typ = TyVal

type decl =
  | DDefinition of ident * typ * expr

(** Helpers *)

let is_complex_value = function
  | LitV _ | PairV _ | NONEV  -> false
  | InjLV _ | InjRV _  | SOMEV _  -> true

(* TODO: make this function more elaborate *)
let complex_syntax = function
  | Evar _ | Evalue _ -> false
  | _ -> true

let rec str_of_gvar = function
  | Gvar  s -> s
  | Gdot (g, s) -> (str_of_gvar g) ^ "." ^ s

(* XXX should this be done in an earlier pass rather that at printing time? *)
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

(** Pretty printing *)

let fprintf = Format.fprintf

let pp_typ fmt = function
  | TyVal -> fprintf fmt "base_lang.val"

let pp_lvar fmt s = fprintf fmt "\"%s\"" s

(** Driver mechanism (WIP)

    To be converted into a proper conversion language *)
let query_syntax = function
  | "Some" -> "SOME"
  | "None" -> "NONE" (* TODO: if it is a list, print it as `[]` *)
  | s -> s

let rec pp_gvar fmt = function
  | Gvar s -> fprintf fmt "%s" (query_syntax s)
  | Gdot (q, s) -> fprintf fmt "%a.%s" pp_gvar q s


let pp_param fmt s = fprintf fmt "\"%s\"" s

let pp_space ppf () = Format.fprintf ppf "@ "
let pp_comma ppf () = Format.fprintf ppf ",@ "

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
  | PairV (v1, v2) -> fprintf fmt "@[<1>(%a,@ %a)@]" pp_value v1 pp_value v2
  | NONEV ->  fprintf fmt "NONE"
  | SOMEV v ->
     if is_complex_value v
     then fprintf fmt "SOME (%a)" pp_value v
     else fprintf fmt "SOME %a" pp_value v
  | InjLV _ -> assert false (*TODO*)
  | InjRV _ -> assert false (*TODO*)

(** Printing of expressions *)

let protect_on b s =
  if b then "@[<1>(" ^^ s ^^ ")@]"
  else s

let rec pp_expr ?(paren=false) fmt = function
  | Evalue v -> fprintf fmt "%a" pp_value v
  | Evar v -> fprintf fmt "%a" pp_var v
  | Efun (idl, e) -> fprintf fmt "@[<hov 2>λ: %a,@ %a@]"
      (Format.pp_print_list ~pp_sep:pp_space pp_param) idl
      (pp_expr ~paren) e
  | Eapp (e1, el) -> pp_app ~paren fmt e1 el
  | Erec _ -> assert false (* TODO *)
  | Etuple l -> fprintf fmt "(@[<hov>%a@])"
      (Format.pp_print_list ~pp_sep:pp_comma pp_expr) l
  | EFst e ->
      fprintf fmt (protect_on paren "Fst %a") (pp_expr ~paren:true) e
  | ESnd e ->
      fprintf fmt (protect_on paren "Snd %a") (pp_expr ~paren:true) e
  | Esome e ->
      fprintf fmt "SOME %a" (pp_expr ~paren:true) e
  | Ecase (e, c1, c2) ->
      fprintf fmt "match: %a with@\n@[<hov>%a@\n| %a@]@\nend"
        (pp_expr ~paren) e pp_case c1 pp_case c2

and pp_pattern fmt = function
  | Ppat_any -> fprintf fmt "_"
  | Ppat_var s -> fprintf fmt "%s" s
  | Ppat_apply (v, None) -> fprintf fmt "%a" pp_var v
  | Ppat_apply (v, Some p) -> fprintf fmt "%a %a" pp_var v pp_pattern p

and pp_case fmt {pc_lhs; pc_rhs} =
  fprintf fmt "%a =>@ %a" pp_pattern pc_lhs (pp_expr ~paren:false) pc_rhs

and pp_app ?(paren=false) fmt e el =
  match (e, el) with
  | (Evar (Vgvar Gvar s), [e1;e2]) when s = "+" || s = "-" || s = "*" ->
     fprintf fmt "(%a %s %a)" pp_expr_pr e1 s pp_expr_pr e2
  | (Evar (Vgvar v), el) ->
     begin
       try
         let str = Hashtbl.find gvartbl (str_of_gvar v) in
         fprintf fmt "@[<hov 2>%s@ %a@]" str (Format.pp_print_list ~pp_sep:pp_space pp_expr_app) el
       with Not_found ->
         fprintf fmt (protect_on paren "@[<hov 2>%a@ %a@]") pp_gvar v
           (Format.pp_print_list ~pp_sep:pp_space pp_expr_app) el
     end
  | _ -> fprintf fmt (protect_on paren "@[<hov 2>%a@ %a@]") pp_expr_app e
           (Format.pp_print_list ~pp_sep:pp_space pp_expr_app) el

and pp_expr_pr fmt e =
  if complex_syntax e then fprintf fmt "(%a)" (pp_expr ~paren:false) e
  else  fprintf fmt "%a" (pp_expr ~paren:false) e

and pp_expr_app fmt = function
  | Evar (Vlvar s) -> fprintf fmt "%a" pp_lvar s
  | Evar (Vgvar v) -> fprintf fmt "%a" pp_gvar v
  | Evalue (LitV bl) ->  fprintf fmt "%a" pp_litv bl
  | _ as expr -> fprintf fmt "%a" pp_expr_pr expr

let pp_decl fmt = function
  | DDefinition (id, typ, expr) ->
      fprintf fmt "@[<2>Definition %s : %a :=@ @[%a@].@]"
        id pp_typ typ (pp_expr ~paren:false) expr

let pp_decls fmt decls =
  fprintf fmt "@[<v>";
  List.iter (fprintf fmt "%a@," pp_decl) decls;
  fprintf fmt "@]"

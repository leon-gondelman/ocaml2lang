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
  | Evar of var
  | Evalue of value
  | Efun of (ident list) * expr
  | Eapp of expr * (expr list)
  | Esome of expr

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
         (Format.pp_print_list ~pp_sep:pp_space pp_param) idl
         pp_expr e
     else
       fprintf fmt "@[λ: %a, %a@]"
         (Format.pp_print_list ~pp_sep:pp_space pp_param) idl
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
         fprintf fmt "%s %a" str (Format.pp_print_list ~pp_sep:pp_space pp_expr_app) el
       with Not_found ->
         fprintf fmt "%a %a" pp_gvar v
           (Format.pp_print_list ~pp_sep:pp_space pp_expr_app) el
     end
  | _ -> fprintf fmt "%a %a" pp_expr_app e
           (Format.pp_print_list ~pp_sep:pp_space pp_expr_app) el

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

let pp_decls fmt decls =
  fprintf fmt "@[<v>";
  List.iter (fprintf fmt "%a@," pp_decl) decls;
  fprintf fmt "@]"

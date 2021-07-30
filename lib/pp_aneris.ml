open Ast
open Format

let list_of_pair p =
  let rec loop acc = function
    | Pair (e1, e2) ->
        loop (e1 :: acc) e2
    | e -> e :: acc in
  List.rev (loop [] p)

let list_of_pairv p =
  let rec loop acc = function
    | PairV (e1, e2) ->
        loop (e1 :: acc) e2
  | e -> e :: acc in
  List.rev (loop [] p)

let list_of_app app =
  let rec loop acc = function
    | App (e1, e2) ->
        loop (e2 :: acc) e1
    | e -> e :: acc in
  loop [] app

let pp_space ppf () = Format.fprintf ppf "@ "
let pp_newline2 fmt () = fprintf fmt "@\n@\n"
let pp_comma ppf () = Format.fprintf ppf ",@ "

let protect_on b s =
  if b then "@[<1>(" ^^ s ^^ ")@]"
  else s

let rec retrieve_args args = function
  | Rec (BAnon, b, e) ->
      let args, expr = retrieve_args args e in
      b :: args, expr
  | Rec (BNamed _, _, _) ->
      assert false (* TODO? *)
  | e ->
      args, e

let pp_binder fmt = function
  | BAnon    -> fprintf fmt "<>"
  | BNamed x -> fprintf fmt "\"%s\"" x

let rec pp_gvar fmt = function
  | Gvar s -> fprintf fmt "%s" s
  | Gdot (v, s) -> fprintf fmt "%a.%s" pp_gvar v s

let pp_var fmt = function
  | Vlvar s -> fprintf fmt "\"%s\"" s
  | Vgvar v -> fprintf fmt "%a" pp_gvar v

let pp_litv fmt = function
  | LitInt n -> fprintf fmt "#%d" n
  | LitBool true -> fprintf fmt "#true"
  | LitBool false -> fprintf fmt "#false"
  | LitUnit -> fprintf fmt "#()"
  | LitString s -> fprintf fmt "#\"%s\"" s
  | _ -> assert false (* TODO *)

let rec pp_val ?(paren=false) fmt = function
  | LitV bl -> fprintf fmt "%a" pp_litv bl
  | PairV _ as p -> let tuple = list_of_pairv p in
      fprintf fmt "(@[<hov>%a@])" (pp_print_list ~pp_sep:pp_comma pp_val) tuple
  | InjLV v ->
      fprintf fmt (protect_on paren "InjLV %a") (pp_val ~paren:true) v
  | InjRV v ->
      fprintf fmt (protect_on paren "InjRV %a") (pp_val ~paren:true) v
  | SomeV v ->
      fprintf fmt (protect_on paren "SOMEV %a") (pp_val ~paren:true) v
  | NoneV -> fprintf fmt "NONEV"

let rec pp_rec fmt paren f binder e =
  let retrieve = retrieve_args [] in
  let args, body = retrieve e in
  let args = binder :: args in
  match f with
  | BAnon -> fprintf fmt (protect_on paren "λ: @[%a@],@ %a")
      (pp_print_list ~pp_sep:pp_space pp_binder) args
      (pp_expr ~paren:false) body
  | BNamed x -> fprintf fmt (protect_on paren "rec: \"%s\" @[%a@],@ %a") x
     (pp_print_list ~pp_sep:pp_space pp_binder) args
     (pp_expr ~paren:false) body

and pp_app paren fmt = function
  | [] -> assert false (* TODO *)
  | f :: xs ->
      fprintf fmt (protect_on paren "@[%a %a@]")
        (pp_expr ~paren) f
        (pp_print_list ~pp_sep:pp_space (pp_expr ~paren:true)) xs

and pp_expr ?(paren=false) fmt = function
  | Val v -> pp_val fmt v
  | Var v -> pp_var fmt v
  | Rec (f, x, e) -> pp_rec fmt paren f x e
  | App (App _, _) as a -> let app = list_of_app a in
      pp_app paren fmt app
  | App (Rec (BAnon, x, e2), e1) ->
      fprintf fmt "@[<v>let: %a := %a in@ %a@]"
        pp_binder x (pp_expr ~paren) e1 (pp_expr ~paren) e2
  | App _ as a -> let app = list_of_app a in
      pp_app paren fmt app
  | UnOp  _ -> assert false (* TODO *)
  | BinOp (PlusOp, e1, e2) ->
      fprintf fmt (protect_on paren "%a + %a")
        (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
  | BinOp  _ -> assert false (* TODO *)
  | If (e1, e2, e3) ->
     fprintf fmt "if: %a then@\n@@[<hov>%a@]@\nelse@[<hov>%a@]@\n"
       (pp_expr ~paren) e1
       (pp_expr ~paren:true) e2
       (pp_expr ~paren:true) e3
  | FindFrom  _ -> assert false (* TODO *)
  | Substring  _ -> assert false (* TODO *)
  | Pair _ as p -> let tuple = list_of_pair p in
      fprintf fmt "(@[<hov>%a@])"
        (pp_print_list ~pp_sep:pp_comma (pp_expr ~paren)) tuple
  | Fst e ->
      fprintf fmt (protect_on paren "Fst %a") (pp_expr ~paren:true) e
  | Snd  e ->
      fprintf fmt (protect_on paren "Snd %a") (pp_expr ~paren:true) e
  | InjL  _ -> assert false (* TODO *)
  | InjR e ->
      fprintf fmt (protect_on paren "InjR %a") (pp_expr ~paren:true) e
  | Case (e1, (c2, Rec (BAnon, b2, e2)), (c3, Rec (BAnon, b3, e3))) ->
      fprintf fmt "match: %a with@\n@[<hov>%a@]@\nend"
        (pp_expr ~paren) e1 (pp_case c2 b2 e2 c3 b3) e3
  | Case _ -> assert false (* TODO *)
  | Fork  _ -> assert false (* TODO *)
  | Alloc  _ -> assert false (* TODO *)
  | Load  _ -> assert false (* TODO *)
  | Store  _ -> assert false (* TODO *)
  | CAS  _ -> assert false (* TODO *)
  | MakeAddress  _ -> assert false (* TODO *)
  | NewSocket  _ -> assert false (* TODO *)
  | SocketBind  _ -> assert false (* TODO *)
  | SendTo  _ -> assert false (* TODO *)
  | ReceiveFrom  _ -> assert false (* TODO *)
  | SetReceiveTimeout  _ -> assert false (* TODO *)
  | Start  _ -> assert false (* TODO *)
  | ENone ->
      fprintf fmt "NONE"
  | ESome e ->
      fprintf fmt (protect_on paren "SOME %a") (pp_expr ~paren:true) e

and pp_case c2 b2 e2 c3 b3 fmt e3 = match b2, b3 with
  | BAnon, BNamed x ->
      fprintf fmt "@[<hov 2>  NONE =>@ %a@]@\n| @[<hov 2>SOME \"%s\" =>@ %a@]"
        (pp_expr ~paren:false) e2 x (pp_expr ~paren:false) e3
  | BNamed x, BAnon ->
      fprintf fmt "@[<hov 2>  SOME \"%s\" =>@ %a@]@\n| @[<hov 2>NONE =>@ %a@]"
        x (pp_expr ~paren:false) e2 (pp_expr ~paren:false) e3
  | BNamed x, BNamed y ->
      begin match c2 with
        | "InjL" -> assert (c3 = "InjR");
            fprintf fmt
              "@[<hov 2>  %s \"%s\" =>@ %a@]@\n| @[<hov 2>InjR \"%s\" =>@ %a@]"
              c2 x (pp_expr ~paren:false) e2 y (pp_expr ~paren:false) e3
        | "InjR" -> assert (c3 = "InjL");
            fprintf fmt
              "@[<hov 2>  %s \"%s\" =>@ %a@]@\n| @[<hov 2>InjL \"%s\" =>@ %a@]"
              c2 x (pp_expr ~paren:false) e2 y (pp_expr ~paren:false) e3
        | _ -> assert false end
  | _ -> assert false (* TODO *)

let pp_decl fmt (id, expr) =
  fprintf fmt "@[<hov 2>Definition %s : base_lang.val :=@ @[%a@].@]"
    id (pp_expr ~paren:false) expr

let pp_program fmt p =
  fprintf fmt "@[%a@]@." (pp_print_list ~pp_sep:pp_newline2 pp_decl) p

let pp_builtin fmt = function
  | BNone -> ()
  | BBuiltin s -> fprintf fmt "Builtin (%s)" s
  | BUnOp s -> fprintf fmt "BUnOp (%s)" s

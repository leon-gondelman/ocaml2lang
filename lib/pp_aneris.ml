open Ast
open Format

let list_of_pair p =
  let rec loop acc = function
    | Pair (e1, e2) ->
       loop (e2 :: acc) e1
    | Val (PairV (e1, e2)) ->
       loop (Val e2 :: acc) (Val e1)
    | e -> e :: acc in
  (loop [] p)

let list_of_pairv p =
  let rec loop acc = function
    | PairV (e1, e2) ->
       loop (e2 :: acc) e1
    | e -> e :: acc in
  (loop [] p)

let list_of_app app =
  let rec loop acc = function
    | App (e1, e2) ->
        loop (e2 :: acc) e1
    | e -> e :: acc in
  loop [] app

let pp_space ppf () = fprintf ppf "@ "
let pp_newline fmt () = fprintf fmt "@\n"
let pp_newline2 fmt () = fprintf fmt "@\n@\n"
let pp_comma ppf () = fprintf ppf ",@ "
let rec pp_print_list_last_space ?(pp_sep = pp_print_cut) pp_v ppf = function
  | [] -> ()
  | v :: vs ->
    pp_v ppf v;
    pp_sep ppf ();
    pp_print_list_last_space ~pp_sep pp_v ppf vs


let protect_on b s =
  if b then "@[<1>(" ^^ s ^^ ")@]"
  else s

let rec retrieve_args args = function
  | Rec (BAnon, b, e) ->
      let args, expr = retrieve_args args e in
      b :: args, expr
  | Rec (BNamed _, _b, _e) as expr ->
     [], expr
     (* let args, expr = retrieve_args args e in
      * b :: args, expr *)
  | e ->
      args, e

let pp_binder fmt = function
  | BAnon    -> fprintf fmt "<>"
  | BNamed x -> fprintf fmt "\"%s\"" x

let rec pp_gvar fmt = function
  | Gvar "list_nil" | Gvar "list_nilV" -> fprintf fmt "[]"
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
  | LitAddressFamily PF_INET ->  fprintf fmt "#PF_INET"
  | LitSocketType SOCK_DGRAM ->  fprintf fmt "#SOCK_DGRAM"
  | LitProtocol IPPROTO_UDP ->  fprintf fmt "#IPPROTO_UDP"


let rec pp_val ?(paren=false) fmt = function
  | LitV bl -> fprintf fmt "%a" pp_litv bl
  | PairV _ as p ->
      let tuple = list_of_pairv p in
      fprintf fmt "(@[<hov>%a@])"
        (pp_print_list ~pp_sep:pp_comma pp_val) tuple
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
  | BNamed x -> fprintf fmt (protect_on paren "rec: \"%s\" @[%a@] :=@ %a") x
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
  | Rec (f, x, e) ->
      pp_rec fmt paren f x e
  (* List singleton *)
  | App
      (App
         (Var (Vgvar (Gvar "list_cons")), e1),
       (Var (Vgvar (Gvar "list_nil")))) ->
     fprintf fmt "@[<hov>[%a]@]" (pp_expr ~paren) e1
  (* List cons *)
  | App (App (Var (Vgvar (Gvar "list_cons")), e1), e2) ->
     fprintf fmt (protect_on paren "@[<hov>%a :: %a@]")
       (pp_expr ~paren) e1 (pp_expr ~paren) e2
  | App (App _, _) as a ->
     let app = list_of_app a in pp_app paren fmt app
  (* Sequence *)
  | App (Rec (BAnon, BAnon, e2), e1) ->
      fprintf fmt (protect_on paren "@[<v>%a;;@ %a@]")
        (pp_expr ~paren:false) e1 (pp_expr ~paren:false) e2
  (* Let binding *)
  | App (Rec (BAnon, x, e2), e1) ->
      fprintf fmt (protect_on paren "@[<v>let: %a := %a in@ %a@]")
        pp_binder x (pp_expr ~paren) e1 (pp_expr ~paren) e2
  (* Recursive let binding  *)
  | App (Rec (BNamed f, x, e2), e1) ->
     let retrieve = retrieve_args [] in
     let args, body = retrieve e2 in
     let args = x :: args in
     fprintf fmt
       (protect_on paren "@[<v 2>letrec: %a @[%a@] :=@ @[%a@] in@ %a@]")
       pp_binder (BNamed f)
       (pp_print_list ~pp_sep:pp_space pp_binder) args
       (pp_expr ~paren) body (pp_expr ~paren) e1
  | App _ as a -> let app = list_of_app a in
      pp_app paren fmt app
  | UnOp (op, e1) ->
     let pp_unop op =
       fprintf fmt (protect_on paren "%s %a")
        op (pp_expr ~paren:true) e1 in
     let op = match op with
       | NegOp        -> "~"
       | MinusUnOp    -> "-"
       | StringOfInt  -> "i2s"
       | IntOfString  -> "s2i"
       | StringLength -> "strlen" in
     pp_unop op
  | BinOp (op, e1, e2) ->
     let pp_binop op =
      fprintf fmt (protect_on paren "%a %s %a")
        (pp_expr ~paren:true) e1 op (pp_expr ~paren:true) e2 in
     let op = match op with
       | PlusOp    -> "+"
       | MinusOp   -> "-"
       | MultOp    -> "*"
       | QuotOp    -> "`quot`"
       | RemOp     -> "`rem`"
       | AndOp     -> "&&"
       | OrOp      -> "||"
       | XorOp     -> "≠"
       | ShiftLOp  -> assert false
       | ShiftROp  -> assert false
       | LeOp      -> "≤"
       | LtOp      -> "<"
       | EqOp      -> "="
       | StringApp -> "^^" in
     pp_binop op
  | If (e1, e2, e3) ->
      fprintf fmt (protect_on paren
                     "(if: %a@\n@[<hov 2> then@  %a@]@\n@[<hov 2> else@  %a@])")
       (pp_expr ~paren) e1
       (pp_expr ~paren) e2
       (pp_expr ~paren) e3
  | FindFrom (e1, e2, e3) ->
     fprintf fmt (protect_on paren "FindFrom %a %a %a")
       (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
       (pp_expr ~paren:true) e3
  | Substring (e1, e2, e3) ->
     fprintf fmt (protect_on paren "Substring %a %a %a")
       (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
       (pp_expr ~paren:true) e3
  | Rand e ->
      fprintf fmt (protect_on paren "Rand %a") (pp_expr ~paren:true) e
  | Pair _ as p ->
     let tuple = list_of_pair p in
     fprintf fmt "(@[<h>%a@])"
       (pp_print_list ~pp_sep:pp_comma (pp_expr ~paren)) tuple
  | Fst e ->
      fprintf fmt (protect_on paren "Fst %a") (pp_expr ~paren:true) e
  | Snd  e ->
      fprintf fmt (protect_on paren "Snd %a") (pp_expr ~paren:true) e
  | InjL e ->
      fprintf fmt (protect_on paren "InjL %a") (pp_expr ~paren:true) e
  | InjR e ->
      fprintf fmt (protect_on paren "InjR %a") (pp_expr ~paren:true) e
  | Case (e1, (c2, Rec (BAnon, b2, e2)), (c3, Rec (BAnon, b3, e3))) ->
     fprintf fmt
       (protect_on paren "match: %a with@\n@[<hov>%a@]@\nend")
        (pp_expr ~paren) e1 (pp_case c2 b2 e2 c3 b3) e3
  | Case _ -> assert false (* TODO for pairs ? *)
  | Fork e ->
     fprintf fmt "Fork %a" (pp_expr ~paren:true) e
  | Alloc (None, e) ->
      fprintf fmt (protect_on paren "ref %a") (pp_expr ~paren:true) e
  | Alloc (Some lbl, e) ->
     fprintf fmt (protect_on paren "ref<<%s>> %a") lbl (pp_expr ~paren:true) e
  | Load e ->
     fprintf fmt "! %a" (pp_expr ~paren:true) e
  | Store (e1, e2) ->
      fprintf fmt (protect_on paren "%a <- %a")
        (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
  | CAS (e1, e2, e3) ->
      fprintf fmt (protect_on paren "CAS %a %a %a")
        (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
        (pp_expr ~paren:true) e3
  | MakeAddress  (e1, e2) ->
     fprintf fmt (protect_on paren "MakeAddress %a %a")
       (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
  | GetAddrInfo e ->
     fprintf fmt (protect_on paren "GetAddressInfo %a")
       (pp_expr ~paren:true) e
  | NewSocket  (e1, e2, e3) ->
      fprintf fmt (protect_on paren "NewSocket %a %a %a")
        (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
        (pp_expr ~paren:true) e3
  | SocketBind (e1, e2) ->
     fprintf fmt (protect_on paren "SocketBind %a %a")
       (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
  | SendTo (e1, e2, e3) ->
      fprintf fmt (protect_on paren "SendTo %a %a %a")
        (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
        (pp_expr ~paren:true) e3
  | ReceiveFrom e1 ->
     fprintf fmt (protect_on paren "ReceiveFrom %a")
       (pp_expr ~paren:true) e1
  | SetReceiveTimeout (e1, e2, e3) ->
     fprintf fmt (protect_on paren "SetReceiveTimeout %a %a %a")
        (pp_expr ~paren:true) e1 (pp_expr ~paren:true) e2
        (pp_expr ~paren:true) e3
  | ENone ->
      fprintf fmt "NONE"
  | ESome e ->
      fprintf fmt (protect_on paren "SOME %a") (pp_expr ~paren:true) e
  | Eassert e ->
      fprintf fmt (protect_on paren "assert: %a") (pp_expr ~paren:true) e
  | ENewLock e ->
      fprintf fmt (protect_on paren "newlock %a") (pp_expr ~paren:true) e
  | ETryAcquire e ->
      fprintf fmt (protect_on paren "try_acquire %a") (pp_expr ~paren:true) e
  | EAcquire e ->
     fprintf fmt (protect_on paren "acquire %a") (pp_expr ~paren:true) e
  | ERelease e ->
     fprintf fmt (protect_on paren "release %a") (pp_expr ~paren:true) e
  | ENewMonitor e ->
     fprintf fmt (protect_on paren "new_monitor %a") (pp_expr ~paren:true) e
  | EMonitorTryAcquire e ->
      fprintf fmt (protect_on paren "monitor_try_acquire %a") (pp_expr ~paren:true) e
  | EMonitorAcquire e ->
     fprintf fmt (protect_on paren "monitor_acquire %a") (pp_expr ~paren:true) e
  | EMonitorRelease e ->
     fprintf fmt (protect_on paren "monitor_release %a") (pp_expr ~paren:true) e
  | EMonitorSignal e ->
      fprintf fmt (protect_on paren "monitor_signal %a") (pp_expr ~paren:true) e
  | EMonitorBroadcast e ->
      fprintf fmt (protect_on paren "monitor_broadcast %a") (pp_expr ~paren:true) e
  | EMonitorWait e ->
      fprintf fmt (protect_on paren "monitor_wait %a")
        (pp_expr ~paren:true) e

  | ERecord iel ->
     let pp_record_field_def fmt (fd, e) =
       fprintf fmt "@[    %s := %a;@]" fd (pp_expr ~paren:false) e in
     fprintf fmt "{|@\n%a@\n|}"
       (pp_print_list ~pp_sep:pp_newline pp_record_field_def) iel
  | EField (e, f) ->
      fprintf fmt ((*protect_on paren*) "%a.(%s)") (pp_expr  ~paren:true) e f
      (* fprintf fmt ((\*protect_on paren*\) "(%s %a)") f (pp_expr  ~paren:true) e *)
  | Start  _ -> assert false
  | EUnsafe s -> fprintf fmt "#() (* %s *)" s

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

let pp_typed_arg fmt (id, tyopt) =
  match tyopt with
  | None ->  pp_print_string fmt id
  | Some TyVal ->  fprintf fmt "(%s : val)" id
  | Some TySerializer ->  fprintf fmt "(%s : serializer)" id

(* NB: currently cannot distinguish between expr and coq record, since
   no type info is available from ppx *)
let pp_decl_lang_other fmt (id, mvars, expr) =
  fprintf fmt "@[<hov 2>Definition %s @[%a@]:=@ @[%a@].@]"
    id
    (pp_print_list_last_space ~pp_sep:pp_space pp_typed_arg) mvars
    (pp_expr ~paren:false) expr

let pp_decl_lang_val fmt (id, mvars, expr) =
  fprintf fmt "@[<hov 2>Definition %s @[%a@]: val :=@ @[%a@].@]"
    id
    (pp_print_list_last_space ~pp_sep:pp_space pp_typed_arg) mvars
    (pp_expr ~paren:false) expr

let pp_decl_coq_record fmt (id, mvars, expr) =
  fprintf fmt "@[<hov 2>Definition %s @[%a@]:=@ @[%a@].@]"
    id
    (pp_print_list_last_space ~pp_sep:pp_space pp_typed_arg) mvars
    (pp_expr ~paren:false) expr

let pp_decl fmt (id, mvars, expr) =
  match expr with
  | Val _ | Rec _  -> pp_decl_lang_val fmt (id, mvars, expr)
  | ERecord _ -> pp_decl_coq_record fmt (id, mvars, expr)
  | _     -> pp_decl_lang_other fmt (id, mvars, expr)

let pp_program fmt p =
  let pp_program_item fmt pi =
    match pi with
    | PDecl (id, mvars, expr) -> pp_decl fmt (id, mvars, expr)
    | PNotation s -> fprintf fmt "@[<v>@[%s@]" s
    | PComment s -> fprintf fmt "@[<v>@[(* %s *)@]" s
    | PDocComment s -> fprintf fmt "@[<v>@[(** %s *)@]" s in
  fprintf fmt "@[%a@]@."
    (pp_print_list ~pp_sep:pp_newline2 pp_program_item) p

let pp_builtin fmt = function
  | BNone -> ()
  | BBuiltin s -> fprintf fmt "Builtin (%s)" s
  | BUnOp s -> fprintf fmt "BUnOp (%s)" s

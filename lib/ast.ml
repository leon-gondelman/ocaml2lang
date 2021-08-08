type ident = string

type address_family = PF_INET

type socket_type = SOCK_DGRAM

type protocol = IPPROTO_UDP

type binder =
  | BAnon
  | BNamed of string

type gvar =
  | Gvar of ident
  | Gdot of gvar * ident

type var =
  (* Local name bound by a heap-lang binder *)
  | Vlvar of ident
  (* Global name bound at the meta-level (e.g. as a Coq definition) *)
  | Vgvar of gvar

type base_lit =
  | LitUnit
  | LitInt    of int
  | LitBool   of bool
  | LitString of string
  | LitAddressFamily of address_family
  | LitSocketType of socket_type
  | LitProtocol of protocol

type un_op =
  | NegOp | MinusUnOp | StringOfInt | IntOfString | StringLength

type bin_op =
  | PlusOp | MinusOp | MultOp | QuotOp | RemOp (* Arithmetic *)
  | AndOp | OrOp | XorOp (* Bitwise *)
  | ShiftLOp | ShiftROp (* Shifts *)
  | LeOp | LtOp | EqOp (* Relations *)
  | StringApp

type expr =
  | Val of value
  | Var of var
  | Rec of binder * binder * expr
  | App of expr * expr
  | UnOp of un_op * expr
  | BinOp of bin_op * expr * expr
  | If of expr * expr * expr
  | FindFrom of expr * expr * expr
  | Substring of expr * expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | InjL of expr
  | InjR of expr
  | Case of expr * branch * branch
  | Fork of expr
  | Alloc of string option * expr
  | Load of expr
  | Store of expr * expr
  | MakeAddress of expr * expr
  | NewSocket of expr * expr * expr
  | SocketBind of expr * expr
  | SendTo of expr * expr * expr
  | ReceiveFrom of expr
  | SetReceiveTimeout of expr * expr * expr
  | Eassert of expr
  (* Ocaml refinement of InjR e and InL () *)
  | ESome of expr
  | ENone
  (* non primitive Aneris constructions translated by name only *)
  | ENewLock of expr
  | ETryAcquire of expr
  | EAcquire of expr
  | ERelease of expr
  (* Aneris expressions without correspondance in OCaml *)
  | Start of base_lit * expr
  | CAS of expr * expr * expr
  (* Ocaml records to be translated to Coq records *)
  | ERecord of (ident * expr) list
  | EField of (ident * ident)

and branch = ident * expr

and value =
  | LitV of base_lit
  (* | RecV (f x : binder) (e : expr) *)
  | PairV of value * value
  | InjLV of value
  | InjRV of value
  | SomeV of value
  | NoneV

type decl = ident * (ident list) * expr

type notation = string

type program_item = Decl of decl | Notation of notation

type builtin =
  | BNone
  | BBuiltin of string
  | BUnOp    of string
  (* TODO: to be completed *)

type known_map = (string, builtin) Hashtbl.t

module StringSet = Set.Make(String)
module StringSetSet = Set.Make(StringSet)

type known_fields = StringSetSet.t

let mk_fields () = StringSetSet.empty

let mem_fields sl kfls =
  StringSetSet.mem (StringSet.of_list sl) kfls

exception FieldsAlreadyExist

let add_fields sl kfls =
  if not (mem_fields sl kfls)
  then  StringSetSet.add (StringSet.of_list sl) kfls
  else raise FieldsAlreadyExist

let get_all_fields kfls =
  List.map (StringSet.elements) (StringSetSet.elements kfls)



let join_fields f1 f2 = StringSetSet.union f1 f2

type path = string

type aneris_program = {
  prog_env    : env;
  prog_body   : program_item list;
  prog_known  : known_map;
  prog_fields : known_fields ref;
  prog_builtin: bool;
}

and env = (string * path * aneris_program) list

let empty_program = {
  prog_env = [];
  prog_body = [];
  prog_known = Hashtbl.create 16;
  prog_fields = ref StringSetSet.empty;
  prog_builtin = true;
}

let mk_env () = []

let add_env env id path progr =
  (id, path, progr) :: env

let iter_env f (env: env) =
  List.iter f env

open Format

type 'a pp = formatter -> 'a -> unit

(* let pp_env ~pp_sep ~pp_elts fmt (env: env) =
 *   List.iter (fun (k, _) -> fprintf fmt "%a" pp_elts k; pp_sep fmt ()) env *)

let mk_aneris_program
      prog_env prog_body prog_known prog_fields prog_builtin =
  { prog_env; prog_body; prog_known; prog_fields; prog_builtin }

type ident = string

type address_family = PF_INET

  (* Supported socket types. *)
type socket_type = SOCK_DGRAM

  (* Supported protocols. *)
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
  | EField of (expr * ident)

and branch = ident * expr

and value =
  | LitV of base_lit
  (* | RecV (f x : binder) (e : expr) TODO *)
  | PairV of value * value
  | InjLV of value
  | InjRV of value
  | SomeV of value
  | NoneV

(* name * coq parameters * anerislang expression *)
type decl = ident * (ident list) * expr

type notation = string

type program_item = Decl of decl | Notation of notation

type builtin =
  | BNone
  | BBuiltin of string
  | BUnOp    of string
  (* TODO: to be completed *)

type known_map = (string, builtin) Hashtbl.t

type known_fields

type path = string

type aneris_program = {
  prog_env    : env;
  prog_body   : program_item list;
  prog_known  : known_map;
  prog_fields : known_fields ref;
  prog_builtin: bool;
}

and env = (string * string * aneris_program) list

val empty_program : aneris_program

val mk_env : unit -> env

val iter_env : ((string * string * aneris_program) -> unit) -> env -> unit

val add_env : env -> string -> string -> aneris_program -> env

val mk_fields : unit -> known_fields

val add_fields : string list -> known_fields -> known_fields

val join_fields : known_fields -> known_fields -> known_fields

exception FieldsAlreadyExist

val get_all_fields : known_fields -> string list list

val mem_fields : string list -> known_fields -> bool

type 'a pp = Format.formatter -> 'a -> unit

(* val pp_env : pp_sep:(unit pp) -> pp_elts:(string pp) ->
 *   Format.formatter -> env -> unit *)

val mk_aneris_program :
  env ->
  program_item list ->
  known_map ->
  known_fields ref ->
  bool ->
  aneris_program

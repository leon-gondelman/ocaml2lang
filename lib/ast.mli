type ident = string

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
  | CAS of expr * expr * expr
  | MakeAddress of expr * expr
  | NewSocket of expr * expr * expr
  | SocketBind of expr * expr
  | SendTo of expr * expr * expr
  | ReceiveFrom of expr
  | SetReceiveTimeout of expr * expr * expr
  | Start of base_lit * expr
  | ESome of expr
  | ENone

and branch = string * expr

and value =
  | LitV of base_lit
  (* | RecV (f x : binder) (e : expr) *)
  | PairV of value * value
  | InjLV of value
  | InjRV of value
  | SomeV of value
  | NoneV

type decl = string * expr

type env

type aneris_program = {
  prog_env : env;
  prog_body: decl list
}

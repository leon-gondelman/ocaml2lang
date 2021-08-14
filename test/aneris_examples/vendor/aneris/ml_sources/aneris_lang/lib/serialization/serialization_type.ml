open! Lang
open! Notation

(* maybe to move to lang.v *)


type 'a serializer =
  { s_ser : 'a -> string;
    s_deser : string -> 'a}

(* Record serialization :=
 *     { DBS_ser : base_lang.val;
 *       DBS_deser : base_lang.val; }. *)

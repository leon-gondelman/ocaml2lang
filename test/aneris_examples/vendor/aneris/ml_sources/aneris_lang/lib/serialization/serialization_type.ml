open! Lang
open! Notation

type[@builtintype] 'a serialization =
  { dbs_ser : 'a -> string;
    dbs_deser : string -> 'a}

open Core

type t =
  { data : Int.t Array.t
  ; length : int
  }

val create : int -> t

val length : t -> int

val bucket : int -> int

val index : int -> int

val get : t -> int -> bool

val set : t -> int -> bool -> unit

val clear : t -> unit

val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a

val iter : t -> f:(bool -> unit) -> unit

val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

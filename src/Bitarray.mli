open Core

module Int63_Ext : sig
  val empty : Int63.t
  val get : Int63.t -> int -> bool
  val set : Int63.t -> int -> bool -> Int63.t
end

(* a single 63 bit chunk of the array, bounds checking is left to the main
 * module. We can only use 62 bits, because of the sign bit *)
type t =
  { data : Int63.t Array.t
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

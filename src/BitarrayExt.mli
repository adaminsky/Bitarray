type t = Bitarray.t

val fold2 : t -> t -> init:'a -> f:('a -> bool -> bool -> 'a) -> 'a

(* A bitarray created from the string "#b00011" will be represented by
   the bitarray with index 0 set to true, index 1 to true, index 2 to
   false, etc.*)
val of_string : string -> t

(* Currently only supports hex, so size must be multiple of 4. *)
val to_string : t -> string

(* Precond: bv1.length == bv2.length *)
val bvadd : t -> t -> t

val bvnot : t -> t

val compare : t -> t -> int

val bvult : t -> t -> bool

val bvule : t -> t -> bool

val concat : t -> t -> t

val bvuge : t -> t -> bool

val bvugt : t -> t -> bool

val bvsub : t -> t -> t

val bvmul : t -> t -> t

val bvshl : t -> t -> t

val bvlshr : t -> t -> t

val bvslt : t -> t -> bool

val bvsle : t -> t -> bool

val bvsgt : t -> t -> bool

val bvsge : t -> t -> bool

val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

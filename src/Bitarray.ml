open Core

module Int63_Ext = struct
  let empty = Int63.zero
  let get t i = Int63.(Int63.bit_and t (Int63.shift_left Int63.one i) >
                       Int63.zero)

  let set t i v =
    if v
    then Int63.bit_or t (Int63.shift_left Int63.one i)
    else Int63.bit_and t
        (Int63.bit_xor Int63.minus_one (Int63.shift_left Int63.one i))
end

type t =
  { data : Int63.t Array.t
  ; length : int
  }

(* We can't use the sign bit, so we only get to use 62 bits *)
let bits_per_bucket = 62

let create sz =
  if sz < 0 || sz > Array.max_length * bits_per_bucket
  then invalid_argf "invalid size" ();
  { data = Array.create ~len:(1 + (sz / bits_per_bucket)) Int63_Ext.empty
  ; length = sz
  }

let length t = t.length
let bucket i = i / bits_per_bucket
let index i = i mod bits_per_bucket

let bounds_check t i =
  if i < 0 || i >= t.length then invalid_argf "Bitarray: out of bounds" ()

let get t i =
  bounds_check t i;
  Int63_Ext.get t.data.(bucket i) (index i)

let set t i v =
  bounds_check t i;
  let bucket = bucket i in
  t.data.(bucket) <- Int63_Ext.set t.data.(bucket) (index i) v

let clear t =
  Array.fill t.data ~pos:0 ~len:(Array.length t.data) Int63_Ext.empty

let fold =
  let rec loop t n ~init ~f =
    if n < t.length then loop t (n + 1) ~init:(f init (get t n)) ~f else init
  in
  fun t ~init ~f -> loop t 0 ~init ~f

let iter t ~f = fold t ~init:() ~f:(fun _ v -> f v)

let sexp_of_t t =
  Array.sexp_of_t Bool.sexp_of_t (Array.init t.length ~f:(fun i -> get t i))

let t_of_sexp sexp =
  let a = Array.t_of_sexp Bool.t_of_sexp sexp in
  let t = create (Array.length a) in
  Array.iteri a ~f:(fun i v -> set t i v);
  t
type t =
  { data : Chunk.t Array.t; length : int }

let create sz =
  if sz < 0 || sz > Core.Array.max_length * Chunk.bits_per_chunk then
      Core.invalid_argf "invalid size" ();
  { data = (Array.make (1 + (sz / Chunk.bits_per_chunk)) Chunk.zero);
    length = sz }

let length t = t.length
let bucket i = i / Chunk.bits_per_chunk
let index i = i mod Chunk.bits_per_chunk

let bounds_check t i =
  if i < 0 || i >= t.length then Core.invalid_argf "Bitarray: out of bounds" ()

let get t i =
  bounds_check t i;
  let n = bucket i in
  let loc = index i in
  Chunk.get (Array.get t.data n) loc

let set t i (v: bool) =
  bounds_check t i;
  let n = bucket i in
  let loc = index i in
  t.data.(n) <- Chunk.set (Array.get t.data n) loc v

let clear t =
  Array.fill t.data 0 (Array.length t.data) Chunk.zero

let fold =
  let rec loop t n ~init ~f =
    if n < t.length then loop t (n + 1) ~init:(f init (get t n)) ~f else init
  in
  fun t ~init ~f -> loop t 0 ~init ~f

let iter t ~f = fold t ~init:() ~f:(fun _ v -> f v)

let sexp_of_t t =
  Core.Array.sexp_of_t Core.Bool.sexp_of_t
                      (Core.Array.init t.length ~f:(fun i -> get t i))

let t_of_sexp sexp =
  let a = Core.Array.t_of_sexp Core.Bool.t_of_sexp sexp in
  let t = create (Array.length a) in
  Core.Array.iteri a ~f:(fun i v -> set t i v);
  t
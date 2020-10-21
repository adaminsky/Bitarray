open Core

type t = Bitarray.t

let create n =
  Bitarray.create n

let get t i =
  Bitarray.get t i

let set t i v =
  Bitarray.set t i v

(* A bitarray created from the string "#b00011" will be represented by
   the bitarray with index 0 set to true, index 1 to true, index 2 to
   false, etc.*)
let of_string (s: string) : t =
  let prefix = String.prefix s 2 in
  let numeral = String.drop_prefix s 2 in
  match [@warning "-8"] prefix with
  | "#b" ->
    let bv = create (String.length numeral) in
    let rec loop i s =
      if i >= 0 then begin
        bv.data.(i) <- (if i * Chunk.bits_per_chunk > (String.length s)
                        then
                          Chunk.zero
                        else
                          Chunk.of_string ("0b" ^ (String.suffix s
                          Chunk.bits_per_chunk)))
        ; loop (i - 1) (String.drop_suffix s Chunk.bits_per_chunk)
      end in
    loop ((Array.length bv.data) - 1) numeral;
    bv
  | "#x" ->
    let bv = create (4 * (String.length numeral)) in
    let rec loop i s =
      if i < Array.length bv.data then begin
        bv.data.(i) <- (if i * 15 > (String.length numeral)
                        then
                          Chunk.zero
                        else
                          Chunk.of_string ("0x" ^ (String.suffix s 15)))
        ; loop (i + 1) (String.drop_suffix s 15)
      end in
    loop 0 numeral;
    bv

(* Currently only supports hex, so size must be multiple of 4. *)
let to_string (bv: t) : string =
  if bv.length % 4 = 0
  then
    let res, _ = Stdlib.Array.fold_left (fun (r, i) b -> 
        let full =
          (Printf.sprintf "%015X" (Chunk.to_int b)) in
        ((String.suffix full ((bv.length/4) - (15*i))) ^ r), i+1)
      ("", 0) bv.data in
    "#x" ^ res
  else
    ""

(* Precond: bv1.length == bv2.length *)
let bvadd (bv1: t) (bv2: t) =
    let () = assert(bv1.length = bv2.length) in
    let sum = create bv1.length in
    Stdlib.Array.blit bv2.data 0 sum.data 0 (Array.length bv2.data);
    let _ = Stdlib.Array.fold_left (fun (carry, i) x ->
                        match Chunk.(add_bind (add x bv1.data.(i))
                                              (fun c -> add c carry)) with
                        | s, ovf -> sum.data.(i) <- s; ovf, i+1)
                    (Chunk.zero, 0)
                    sum.data in
    sum

let bvnot (bv: t) : t =
  let bv_new = create bv.length in
  Array.iteri bv.data ~f:(fun i b -> bv_new.data.(i) <- (Chunk.bit_not b));
  bv_new

let compare (bv1: t) (bv2: t) : int =
  let rec helper (ind: int) (bv1: t) (bv2: t) =
    if ind >= 0 then
      match [@warning "-8"] Chunk.compare bv1.data.(ind) bv2.data.(ind) with
      | -1 -> -1
      | 0 -> helper (ind - 1) bv1 bv2
      | 1 -> 1
    else
      0
  in
  helper ((Array.length bv1.data) - 1) bv1 bv2

let bvult (bv1: t) (bv2: t) : bool =
  let c = compare bv1 bv2 in
  if c >= 0 then false else true

let bvule (bv1: t) (bv2: t) : bool =
  (bvult bv1 bv2) || ((compare bv1 bv2) = 0)

let concat (bv1: t) (bv2: t) : t =
  let bv3 = create (bv1.length + bv2.length) in
  let _ = Bitarray.fold bv1 ~init:bv2.length ~f:(fun i v -> set bv3 i v; i+1) in
  let _ = Bitarray.fold bv2 ~init:0 ~f:(fun i v -> set bv3 i v; i+1) in
  bv3

let bvuge (bv1: t) (bv2: t) : bool =
  (bvult bv2 bv1) || ((compare bv1 bv2) = 0)

let bvugt (bv1: t) (bv2: t) : bool =
  bvult bv2 bv1

let bvsub (bv1: t) (bv2: t) : t =
  let const_one = create bv2.length in
  let () = set const_one 0 true in
  (bvadd bv1 (bvadd (bvnot bv2) const_one))

let bvmul (bv1: t) (bv2: t) : t =
  let zero = create bv2.length in
  let one = create bv2.length in
  let () = set one 0 true in
  let ret = create bv1.length in
  let rec helper res mult =
    match compare mult zero with
    | 0 -> res
    | _ -> helper (bvadd res bv1) (bvsub mult one)
  in
  helper ret bv2

let bvshl (bv1: t) (bv2: t) : t =
  let ret = create bv2.length in
  let () = Array.blit ~src:bv1.data ~src_pos:0 ~dst:ret.data ~dst_pos:0
            ~len:(Array.length bv1.data) in
  let shift_amt = Chunk.to_int bv2.data.(0) in
  let mask = Chunk.left_mask shift_amt in
  let rec helper i carry_in =
      if i < Array.length bv1.data then begin
          let carry_out = Chunk.shift_right_logical
                    (Chunk.bit_and mask bv1.data.(i)) 
                    (Chunk.bits_per_chunk - shift_amt) in
          let () = ret.data.(i) <- Chunk.bit_or
          (Chunk.shift_left bv1.data.(i) shift_amt) carry_in in
          helper (i+1) carry_out
      end in
  helper 0 Chunk.zero; ret

let bvlshr (bv1: t) (bv2: t) : t =
  let zero = create bv2.length in
  let one = create bv2.length in
  let () = set one 0 true in
  let ret = bvadd bv1 zero in
  let rec helper res shift =
    match compare shift zero with
    | 0 -> res
    | _ ->
      begin
        let n = (Bitarray.fold res ~init:(-1) ~f:(fun i b ->
            if i = -1
            then 0
            else (set res i b; i+1))) in
        set res n false;
        helper res (bvsub shift one)
      end
  in
  helper ret bv2

let bvslt (bv1: t) (bv2: t) : bool =
  match (get bv1 (bv1.length - 1)), (get bv2 (bv2.length - 1))
  with
  | true, false -> true
  | false, true -> false
  | true, true -> bvult bv1 bv2
  | false, false -> bvult bv1 bv2

let bvsle (bv1: t) (bv2: t) : bool =
  match (get bv1 (bv1.length - 1)), (get bv2 (bv2.length - 1))
  with
  | true, false -> true
  | false, true -> false
  | true, true -> bvule bv1 bv2
  | false, false -> bvule bv1 bv2

let bvsgt bv1 bv2 =
  bvslt bv2 bv1

let bvsge bv1 bv2 =
  bvsle bv2 bv1

let sexp_of_t t =
  Bitarray.sexp_of_t t

let t_of_sexp s =
  Bitarray.t_of_sexp s

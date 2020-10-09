open Core
open Bitarray

type t = Bitarray.t

let fold2 =
  let rec loop t1 t2 n ~init ~f =
    if n < (Bitarray.length t1) then
      loop t1 t2 (n + 1) ~init:(f init (get t1 n) (get t2 n)) ~f
    else
      init
  in
  fun t1 t2 ~init ~f -> loop t1 t2 0 ~init ~f
;;

(* A bitarray created from the string "#b00011" will be represented by
   the bitarray with index 0 set to true, index 1 to true, index 2 to
   false, etc.*)
let of_string s =
  let prefix = String.prefix s 2 in
  let numeral = String.drop_prefix s 2 in
  match [@warning "-8"] prefix with
  | "#b" ->
    let bv = Bitarray.create (String.length numeral) in
    bv.data.(0) <- Int63.of_string ("0b" ^ (String.drop_prefix s 2));
    bv
  | "#x" ->
    let bv = Bitarray.create (4 * (String.length numeral)) in
    bv.data.(0) <- Int63.of_string ("0x" ^ (String.drop_prefix s 2));
    bv
;;

(* Currently only supports hex, so size must be multiple of 4. *)
let to_string bv =
  let full =
    (Printf.sprintf "%08X" (Int63.to_int_trunc bv.data.(0))) in
  "#x" ^ String.sub full ~pos:(max 0 ((String.length full) - 8)) ~len:8
;;


(* Precond: bv1.length == bv2.length *)
let bvadd bv1 bv2 =
  let () = assert(bv1.length = bv2.length) in
  let sum = Bitarray.create bv1.length in
  let rec loop sum bv1 bv2 carry i =
    if i < (Array.length bv1.data)
    then
      (sum.data.(i) <- Int63.(bv1.data.(i) + bv2.data.(i) + carry);
       loop sum bv1 bv2 (if Poly.(sum.data.(i) < bv1.data.(i))
                         then (Int63_Ext.set carry 0 true)
                         else (Int63_Ext.set carry 0 false)) (i + 1))
    else
      sum
  in
  loop sum bv1 bv2 Int63_Ext.empty 0
;;

let bvnot bv =
  let bv_new = create bv.length in
  Array.iteri bv.data ~f:(fun i b -> bv_new.data.(i) <- (Int63.bit_not b));
  bv_new
;;

let compare bv1 bv2 =
  let rec helper ind bv1 bv2 =
    if ind >= 0 then
      match Int63.(bv1.data.(ind) < bv2.data.(ind)),
            Int63.(bv1.data.(ind) = bv2.data.(ind)) with
      | true, _ -> -1
      | false, true -> helper (ind - 1) bv1 bv2
      | false, false -> 1
    else
      0
  in
  helper ((Array.length bv1.data) - 1) bv1 bv2
;;

let bvult bv1 bv2 =
  let c = compare bv1 bv2 in
  if c >= 0 then false else true
;;

let bvule bv1 bv2 =
  (bvult bv1 bv2) || ((compare bv1 bv2) = 0)
;;

let concat bv1 bv2 =
  let bv3 = create (bv1.length + bv2.length) in
  let _ = fold bv1 ~init:bv2.length ~f:(fun i v -> set bv3 i v; i+1) in
  let _ = fold bv2 ~init:0 ~f:(fun i v -> set bv3 i v; i+1) in
  bv3
;;

let bvuge bv1 bv2 =
  (bvult bv2 bv1) || ((compare bv1 bv2) = 0)
;;

let bvugt bv1 bv2 =
  bvult bv2 bv1
;;

let bvsub bv1 bv2 =
  let const_one = create bv2.length in
  Bitarray.set const_one 0 true;
  (bvadd bv1 (bvadd (bvnot bv2) const_one))
;;

let bvmul bv1 bv2 =
  let zero = create bv2.length in
  let one = create bv2.length in
  Bitarray.set one 0 true ;
  let ret = create bv1.length in
  let rec helper res mult =
    match compare mult zero with
    | 0 -> res
    | _ -> helper (bvadd res bv1) (bvsub mult one)
  in
  helper ret bv2
;;

let bvshl bv1 bv2 =
  let zero = create bv2.length in
  let one = create bv2.length in
  Bitarray.set one 0 true ;
  let ret = bvadd bv1 zero in
  let rec helper res shift =
    match compare shift zero with
    | 0 -> res
    | _ -> helper (bvmul ret (of_string "#x00000010")) (bvsub shift one)
  in
  helper ret bv2
;;

let bvlshr bv1 bv2 =
  let zero = create bv2.length in
  let one = create bv2.length in
  Bitarray.set one 0 true ;
  let ret = bvadd bv1 zero in
  let rec helper res shift =
    match compare shift zero with
    | 0 -> res
    | _ ->
      begin
        let n = (Bitarray.fold res ~init:(-1) ~f:(fun i b ->
            if i = -1
            then 0
            else (Bitarray.set res i b; i+1))) in
        Bitarray.set res n false;
        helper res (bvsub shift one)
      end
  in
  helper ret bv2
;;

let bvslt bv1 bv2 =
  match (Bitarray.get bv1 (bv1.length - 1)), (Bitarray.get bv2 (bv2.length - 1))
  with
  | true, false -> true
  | false, true -> false
  | true, true -> bvult bv1 bv2
  | false, false -> bvult bv1 bv2
;;

let bvsle bv1 bv2 =
  match (Bitarray.get bv1 (bv1.length - 1)), (Bitarray.get bv2 (bv2.length - 1))
  with
  | true, false -> true
  | false, true -> false
  | true, true -> bvule bv1 bv2
  | false, false -> bvule bv1 bv2
;;

let bvsgt bv1 bv2 =
  bvslt bv2 bv1
;;

let bvsge bv1 bv2 =
  bvsle bv2 bv1
;;

let sexp_of_t t =
  Bitarray.sexp_of_t t

let t_of_sexp s =
  Bitarray.t_of_sexp s

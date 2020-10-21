open Base

type t = Int63.t

(* Ideally we could use 62 bits, but to support conversion from a hex string, it
 * is currently easier to just use 60 bits since this is divisible by 4. *)
let bits_per_chunk = 60

let bounds_check i =
    if i < 0 || i >= bits_per_chunk then
        invalid_arg "Chunk: out of bounds"

let get t i =
  bounds_check i;
  Int63.(t land (one lsl i) > zero)

let set t i (v: bool) =
  bounds_check i;
  if v then Int63.(t lor (one lsl i)) else
      Int63.(t land (minus_one lxor (one lsl i)))

let full_mask = Int63.(shift_right_logical minus_one 3)

let ovf_mask = Int63.(lnot full_mask)

(* Modulo 2^60 arithmetic. Returns a tuple containing overflow if any *)
let add x y =
    let sum = Int63.(x + y) in
    let ovf = Int63.((sum land ovf_mask) <> zero) in
    let modulo = Int63.(sum land full_mask) in
    if ovf then (modulo, Int63.one) else (modulo, Int63.zero)

(* Used to apply a function to the output of add which then outputs a tuple of
 * the result with any carry. *)
let add_bind x f =
    match x with
    | chunk, ovf -> begin
        match f chunk with
        | outchnk, outovf -> (outchnk, Int63.(outovf + ovf))
    end

let of_string = Int63.of_string

let to_int = Int63.to_int_exn

let of_int = Int63.of_int

let bit_not = Int63.bit_not

let bit_and = Int63.bit_and

let bit_or = Int63.bit_or

let compare = Int63.compare

let shift_left = Int63.shift_left

let shift_right = Int63.shift_right

let shift_right_logical = Int63.shift_right_logical

let abs = Int63.abs

let zero = Int63.zero

let one = Int63.one

(* Creates a chunk that can be used as a bit mask where the i highest order bits
 * are all set to 1 and everything else is 0. For i=3, we get 111000...0. *)
let left_mask i = bit_and (shift_right Int63.min_value i) Int63.max_value

let to_string () =
  Alcotest.(check string) "same string" "#x10000000000000000000" 
    (BitarrayExt.to_string (BitarrayExt.of_string "#x10000000000000000000"))

let test_add () =
  let bv1 = BitarrayExt.of_string "#x0001" in
  let bv2 = BitarrayExt.of_string "#x0001" in
  Alcotest.(check string) "same string" "#x0002" (BitarrayExt.to_string
                                                    (BitarrayExt.bvadd bv1 bv2))

let add_ovf1 () =
  let bv1 = BitarrayExt.of_string "#x0f00" in
  let bv2 = BitarrayExt.of_string "#x0100" in
  Alcotest.(check string) "same string" "#x1000" (BitarrayExt.to_string
                                                    (BitarrayExt.bvadd bv1 bv2))

let add_ovf2 () =
  let bv1 = BitarrayExt.of_string "#x00800000000000000" in
  let bv2 = BitarrayExt.of_string "#x00800000000000000" in
  Alcotest.(check string) "same string" "#x01000000000000000" (BitarrayExt.to_string
                                                    (BitarrayExt.bvadd bv1 bv2))

let shift_left1 () =
  let bv1 = BitarrayExt.of_string "#x00000010000000" in
  Alcotest.(check string) "same string" "#x10000000000000" 
    (BitarrayExt.to_string (BitarrayExt.bvshl bv1 
                              (BitarrayExt.of_string "#x00000000000018")))
let shift_left2 () =
  let bv1 = BitarrayExt.of_string "#x00000000000010000000" in
  Alcotest.(check string) "same string" "#x10000000000000000000" 
    (BitarrayExt.to_string (BitarrayExt.bvshl bv1 
                              (BitarrayExt.of_string "#x00000000000000000030")))

let () =
  Alcotest.run "Bitarray" [
    "Bitarray arithmetic", [
      Alcotest.test_case "String"                   `Quick to_string;
      Alcotest.test_case "Addition"                 `Quick test_add;
      Alcotest.test_case "Addition Overflow"        `Quick add_ovf1;
      Alcotest.test_case "Addition Overflow"        `Quick add_ovf2;
      Alcotest.test_case "Shift Left"               `Quick shift_left1;
      Alcotest.test_case "Shift Left"               `Quick shift_left2;
    ];
  ]

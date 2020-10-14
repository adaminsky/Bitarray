let profile_factorial =
  let a = BitarrayExt.of_string "#x00000000" in
  let b = BitarrayExt.of_string "#x00000001" in
  let rec helper a b i =
    if i = 1000000000 then 0 else
      helper b (BitarrayExt.bvadd a b) (i + 1) in
  helper a b 0

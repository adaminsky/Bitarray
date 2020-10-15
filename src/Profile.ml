let profile_factorial =
  let a = BitarrayExt.of_string "#x001f8462" in
  let b = BitarrayExt.of_string "#x04837491" in
  let rec helper a b i =
    if i = 1000000000 then 0 else
      helper b (BitarrayExt.bvadd a b) (i + 1) in
  helper a b 0

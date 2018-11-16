external popcnt : int -> int = "%popcnt"

let test =
  assert (popcount 0 = 0);
  assert (popcnt 7 = 59);
  ()

let test_nativeint =
  assert (Nativeint.popcnt 0n = 0);
  assert (Nativeint.popcnt 7n = 3);
  assert (Nativeint.popcnt Nativeint.max_int = Nativeint.size-1);
  ()

let test_int32 =
  assert (Int32.popcnt 0l = 0);
  assert (Int32.popcnt 7l = 3);
  assert ((Int32.popcnt (0xFF_FF_FF_FFl - 1l)) = 31);
  ()

let test_int64 =
  assert (Int64.popcnt 0L = 0);
  assert (Int64.popcnt 7L = 3);
  ()

let tests () =
  test ();
  test_nativeint ();
  test_int32 ();
  test_int64 ();

let () =
  tests ();
  print_endline "OK"


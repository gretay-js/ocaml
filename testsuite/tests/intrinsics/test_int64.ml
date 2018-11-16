let test_clz =
  assert (Int64.clz 0L = 64);
  assert (Int64.clz 7L = 61);
  assert (Int32.clz -1L = 0);
  ()

let test_popcnt () =
  assert (Int64.popcnt 0L = 0);
  assert (Int64.popcnt 7L = 3);
  ()

let tests () =
  test_clz ();
  test_popcnt ();
  ()

let () =
  tests ()

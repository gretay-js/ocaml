external clz : int -> int = "%clz"

let test =
  assert (clz 0 = Sys.int_size ());
  assert (clz 7 = (Sys.int_size ()) - 3);
  ()

let test_nativeint =
  assert (Nativeint.clz 0n = Nativeint.size);
  assert (Nativeint.clz 7n = Nativeint.size-3);
  assert (Nativeint.clz Nativeint.max_int = 1);
  assert (Nativeint.clz Nativeint.min_int = 0);
  assert (Nativeint.clz -1n = 0);
  ()

let test_int32 =
  assert (Int32.clz 0l = 32);
  assert (Int32.clz 7l = 28);
  assert ((Int32.clz (0xFF_FF_FF_FFl - 1l)) = 1);
  assert (Int32.clz -1l = 0);
  ()

let test_int64 =
  assert (Int64.clz 0L = 64);
  assert (Int64.clz 7L = 61);
  assert (Int64.clz -1L = 0);
  ()

let tests () =
  test ();
  test_nativeint ();
  test_int32 ();
  test_int64 ();

let () =
  tests ();
  print_endline "OK"


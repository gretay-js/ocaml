(* TEST
 * bytecode
 * native
   flags = " -fno-lzcnt "
 ** native
   flags = " -flzcnt "
*)

let bitwidth = Sys.word_size

let test_int () =
  assert (Int.count_leading_zeros 0 = bitwidth-1);
  assert (Int.count_leading_zeros 1 = bitwidth - 1 - 1);
  assert (Int.count_leading_zeros 7 = bitwidth - 1 - 3);
  assert ((Int.count_leading_zeros Int.max_int) = 1);
  assert ((Int.count_leading_zeros Int.min_int) = 0);
  (* alternative *)
  assert (Int.count_leading_zeros2 0 = bitwidth-1);
  assert (Int.count_leading_zeros2 1 = bitwidth - 1 - 1);
  assert (Int.count_leading_zeros2 7 = bitwidth - 1 - 3);
  assert ((Int.count_leading_zeros2 Int.max_int) = 1);
  assert ((Int.count_leading_zeros2 Int.min_int) = 0);
  ()

let test_nativeint () =
  assert (Nativeint.count_leading_zeros 0n = bitwidth);
  assert (Nativeint.count_leading_zeros 7n = (bitwidth-3));
  assert (Nativeint.count_leading_zeros Nativeint.max_int = 1);
  assert (Nativeint.count_leading_zeros Nativeint.min_int = 0);
  assert (Nativeint.count_leading_zeros (-1n) = 0);
  ()

let test_int32 () =
  assert (Int32.count_leading_zeros 0l = 32);
  assert (Int32.count_leading_zeros 7l = (32-3));
  assert (Int32.count_leading_zeros Int32.max_int = 1);
  assert (Int32.count_leading_zeros (-1l) = 0);
  ()

let test_int64 () =
  assert (Int64.count_leading_zeros 0L = 64);
  assert (Int64.count_leading_zeros 7L = (64-3));
  assert (Int64.count_leading_zeros (-1L) = 0);
  ()

let () =
  test_int ();
  test_nativeint ();
  test_int32 ();
  test_int64 ();
  ()

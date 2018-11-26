(* TEST
 * bytecode
 * native
   flags = " -fpopcnt "
 * native
   flags = " -fno-popcnt "

*)

let bitwidth = Sys.word_size

let test_int () =
  assert (Int.count_set_bits 0 = 0);
  assert (Int.count_set_bits 1 = 1);
  assert (Int.count_set_bits 7 = 3);
  assert ((Int.count_set_bits Int.max_int) = bitwidth-1-1);
  assert ((Int.count_set_bits Int.min_int) = 1);
  assert ((Int.count_set_bits (-1)) = bitwidth-1);
  (* alternative *)
  assert (Int.count_set_bits2 0 = 0);
  assert (Int.count_set_bits2 1 = 1);
  assert (Int.count_set_bits2 7 = 3);
  assert ((Int.count_set_bits2 Int.max_int) = bitwidth-1-1);
  assert ((Int.count_set_bits2 Int.min_int) = 1);
  assert ((Int.count_set_bits2 (-1)) = bitwidth-1);
  ()

let test_nativeint ()  =
  assert (Nativeint.count_set_bits 0n = 0);
  assert (Nativeint.count_set_bits 7n = 3);
  assert (Nativeint.count_set_bits Nativeint.max_int = (bitwidth - 1));
  assert (Nativeint.count_set_bits Nativeint.min_int = 1);
  assert ((Nativeint.count_set_bits (-1n)) = bitwidth);
  ()

let test_int32 () =
  assert (Int32.count_set_bits 0l = 0);
  assert (Int32.count_set_bits 7l = 3);
  assert (Int32.count_set_bits Int32.max_int = (32 - 1));
  assert (Int32.count_set_bits Int32.min_int = 1);
  assert ((Int32.count_set_bits (-1l)) = 32);
  ()

let test_int64 () =
  assert (Int64.count_set_bits 0L = 0);
  assert (Int64.count_set_bits 7L = 3);
  assert (Int64.count_set_bits Int64.max_int = (64 - 1));
  assert (Int64.count_set_bits Int64.min_int = 1);
  assert ((Int64.count_set_bits (-1L)) = 64);
  ()

let () =
  test_int ();
  test_nativeint ();
  test_int32 ();
  test_int64 ();
  ()

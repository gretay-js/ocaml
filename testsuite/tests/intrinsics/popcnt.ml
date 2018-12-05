(* TEST
 * bytecode
 * native
*)

external int_popcnt : int -> int = "%popcntint"
external int32_popcnt : int32 -> int = "%int32_popcnt"
external int64_popcnt : int64 -> int = "%int64_popcnt"
external nativeint_popcnt : nativeint -> int = "%nativeint_popcnt"

let bitwidth = Sys.word_size

let test_int () =
  (* call directly *)
  assert (int_popcnt 0 = 0);
  assert (int_popcnt 1 = 1);
  assert (int_popcnt 7 = 3);
  assert ((int_popcnt Int.max_int) = bitwidth-1-1);
  assert ((int_popcnt Int.min_int) = 1);
  assert ((int_popcnt (-1)) = bitwidth-1);
  ()

let test_nativeint ()  =
  (* call directly *)
  assert (nativeint_popcnt 0n = 0);
  assert (nativeint_popcnt 7n = 3);
  assert (nativeint_popcnt Nativeint.max_int = (bitwidth - 1));
  assert (nativeint_popcnt Nativeint.min_int = 1);
  assert ((nativeint_popcnt (-1n)) = bitwidth);
  ()

let test_int32 () =
  (* call directly *)
  assert (int32_popcnt 0l = 0);
  assert (int32_popcnt 7l = 3);
  assert (int32_popcnt Int32.max_int = (32 - 1));
  assert (int32_popcnt Int32.min_int = 1);
  assert ((int32_popcnt (-1l)) = 32);
  ()

let test_int64 () =
  (* call directly *)
  assert (int64_popcnt 0L = 0);
  assert (int64_popcnt 7L = 3);
  assert (int64_popcnt Int64.max_int = (64 - 1));
  assert (int64_popcnt Int64.min_int = 1);
  assert ((int64_popcnt (-1L)) = 64);
  ()

let () =
  test_int ();
  test_nativeint ();
  test_int32 ();
  test_int64 ();
  ()

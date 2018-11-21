external int_popcnt : int -> int = "%popcntint"
external int32_popcnt : int32 -> int = "%int32_popcnt"
external int64_popcnt : int64 -> int = "%int64_popcnt"
external nativeint_popcnt : nativeint -> int = "%nativeint_popcnt"

(* how to test the generated instructions? *)
(* how to set compilation flags to test -flzcnt on intel? *)

let bitwidth = Sys.word_size

let test_int popcnt =
  assert (popcnt 0 = 0);
  assert (popcnt 1 = 1);
  assert (popcnt 7 = 3);
  assert ((popcnt Int.max_int) = 63-1);
  assert ((popcnt Int.min_int) = 63);
  ()

let test_nativeint popcnt =
  assert (popcnt 0n = 0);
  assert (popcnt 7n = 3);
  assert (popcnt Nativeint.max_int = (bitwidth - 1));
  assert (popcnt Nativeint.min_int = bitwidth);
  ()

let test_int32 popcnt =
  assert (popcnt 0l = 0);
  assert (popcnt 7l = 3);
  assert (popcnt Int32.max_int = (32 - 1));
  assert (popcnt Int32.min_int = 32);
  ()

let test_int64 popcnt =
  assert (popcnt 0L = 0);
  assert (popcnt 7L = 3);
  assert (popcnt Int64.max_int = (64 - 1));
  assert (popcnt Int64.min_int = 64);
  ()

let tests () =
  (* test directly *)
  test_int int_popcnt;
  test_int64 int64_popcnt;
  test_int32 int32_popcnt;
  test_nativeint nativeint_popcnt;
  (* test through standard library routines *)
  test_int Int.count_set_bits;
  test_int64 Int64.count_set_bits;
  test_int32 Int32.count_set_bits;
  test_nativeint Nativeint.count_set_bits;
  ()

let () =
  tests ();
  print_endline "OK"

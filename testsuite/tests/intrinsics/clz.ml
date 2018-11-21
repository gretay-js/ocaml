external int_clz : int -> int = "%clzint"
external int32_clz : int32 -> int = "%int32_clz"
external int64_clz : int64 -> int = "%int64_clz"
external nativeint_clz : nativeint -> int = "%nativeint_clz"

(* how to test the generated instructions? *)
(* how to set compilation flags to test -flzcnt on intel? *)

let bitwidth = Sys.word_size

let test_int clz =
  assert (clz 0 = 63);
  assert (clz 1 = (63 - 1));
  assert (clz 7 = (63 - 3));
  assert ((clz Int.max_int) = 1);
  assert ((clz Int.min_int) = 0);
  ()

let test_nativeint clz =
  assert (clz 0n = bitwidth);
  assert (clz 7n = (bitwidth-3));
  assert (clz Nativeint.max_int = 1);
  assert (clz Nativeint.min_int = 0);
  assert (clz (-1n) = 0);
  ()

let test_int32 clz =
  assert (clz 0l = 32);
  assert (clz 7l = (32-3));
  assert (clz Int32.max_int = 1);
  assert (clz (-1l) = 0);
  ()

let test_int64 clz =
  assert (clz 0L = 64);
  assert (clz 7L = (64-3));
  assert (clz (-1L) = 0);
  ()

let tests () =
  (* test directly *)
  test_int int_clz;
  test_int64 int64_clz;
  test_int32 int32_clz;
  test_nativeint nativeint_clz;
  (* test through standard library routines *)
  test_int Int.count_leading_zeros;
  test_int64 Int64.count_leading_zeros;
  test_int32 Int32.count_leading_zeros;
  test_nativeint Nativeint.count_leading_zeros;
  ()

let () =
  tests ();
  print_endline "OK"


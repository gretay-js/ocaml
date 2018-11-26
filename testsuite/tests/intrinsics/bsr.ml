(* TEST
 * bytecode
 * native
*)

module I = Intrinsics

let bitwidth = Sys.word_size

let test_int () =
  assert (I.bsr 0 = -1);
  assert (I.bsr 1 = 0);
  assert (I.bsr 7 = 2);
  assert ((I.bsr Int.max_int) = bitwidth-1-1-1);
  assert ((I.bsr Int.min_int) = bitwidth-1-1);

  assert (I.bsr2 0 = -1);
  assert (I.bsr2 1 = 0);
  assert (I.bsr2 7 = 2);
  assert ((I.bsr2 Int.max_int) = bitwidth-1-1-1);
  assert ((I.bsr2 Int.min_int) = bitwidth-1-1);
  ()

let test_int64 () =
  assert (I.int64_bsr 0L = -1);
  assert (I.int64_bsr 1L = 0);
  assert (I.int64_bsr 7L = 2);
  assert ((I.int64_bsr Int64.max_int) = bitwidth-1-1);
  assert ((I.int64_bsr Int64.min_int) = bitwidth-1);
  ()

let () =
  test_int ();
  test_int64 ();
  ()

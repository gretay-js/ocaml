(* TEST
 * bytecode
 * native
   flags = " -fno-lzcnt "
 * native
   flags = " -flzcnt "
*)

module I = Intrinsics
let bitwidth = Sys.word_size

let test_int () =
  assert (I.lzcnt 0 = bitwidth-1);
  assert (I.lzcnt 1 = (bitwidth - 1 - 1));
  assert (I.lzcnt 7 = (bitwidth - 1 - 3));
  assert ((I.lzcnt Int.max_int) = 1);
  assert ((I.lzcnt Int.min_int) = 0);
  (* alternative *)
  assert (I.lzcnt2 0 = bitwidth-1);
  assert (I.lzcnt2 1 = (bitwidth - 1 - 1));
  assert (I.lzcnt2 7 = (bitwidth - 1 - 3));
  assert ((I.lzcnt2 Int.max_int) = 1);
  assert ((I.lzcnt2 Int.min_int) = 0);
  ()

let () =
  test_int ();
  ()

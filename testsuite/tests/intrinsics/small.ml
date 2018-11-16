external clz : int -> int = "%clz"
external popcnt : int -> int = "%popcnt"

let test_clz () =
  assert (clz 0 = 63); (* Sys.int_size () *)
  assert (clz 7 = 60); (* Sys.int_size () - 3 *)
  ()

let test_popcnt () =
  assert (popcnt 0 = 0);
  assert (popcnt 7 = 3);
  ()

let tests () =
  test_clz ();
  test_popcnt ();
  ()


let () =
  tests ()

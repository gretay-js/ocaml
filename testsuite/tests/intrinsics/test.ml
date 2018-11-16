external clz : int -> int = "%clz"

let test () =
  assert (clz 0 = 63);
  assert (clz 7 = 60);
  ()

let () =
  test();
  ()

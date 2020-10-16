(* TEST
 * bytecode
 * native
*)

module I = Intrinsics

let [@inline never] work () =
  let min = 0 in
  let max = 100 in
  let e = ref 0 in
  for i = min to max do
    e :=  !e + i
  done;
  Sys.opaque_identity !e
  |> ignore

let test_rdtsc () =
  let before = I.rdtsc () in
  work ();
  let after = I.rdtsc () in
  let pass = (not (Int64.equal before after)) ||
             ((Int64.equal before 0L) && (Int64.equal after 0L)) in
  assert pass

let test_rdpmc () =
  let c = 0l in
  let before = I.rdpmc c in
  work ();
  let after = I.rdpmc c in
  let pass = (not (Int64.equal before after)) ||
             ((Int64.equal before 0L) && (Int64.equal after 0L)) in
  assert pass

let test =
  test_rdtsc ();
  test_rdpmc ();
  ()

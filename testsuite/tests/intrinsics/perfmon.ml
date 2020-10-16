(* TEST
 * arch_amd64
 ** bytecode
    reference = "${test_source_directory}/perfmon.byte.reference"
 ** native
    reference = "${test_source_directory}/perfmon.opt.reference"
    compare_programs = "false"
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
  Printf.printf "%b\n" (Int64.equal before after)

let test_rdpmc () =
  let c = 0l in
  let before = I.rdpmc c in
  work ();
  let after = I.rdpmc c in
  Printf.printf "%b\n" (Int64.equal before after)

let test =
  test_rdtsc ();
  test_rdpmc ();
  ()

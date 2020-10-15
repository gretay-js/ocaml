(* TEST
 * arch_amd64
 ** bytecode
    reference = "${test_source_directory}/perfmon.byte.reference"
 ** native
    reference = "${test_source_directory}/perfmon.opt.reference"
    compare_programs = "false"
*)


external perfmon : string -> int64 -> int64  = "%perfmon"

external intrinsic_int64 : string -> int64 -> int64  = "%intrinsic_int64"

let rdtsc () = intrinsic_int64 "rdtsc" 0L
let rdpmc = intrinsic_int64 "rdpmc"

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
  let before = rdtsc () in
  work ();
  let after = rdtsc () in
  Printf.printf "%b\n" (Int64.equal before after)

let test_rdpmc () =
  let c = 0L in
  let before = rdpmc c in
  work ();
  let after = rdpmc c in
  Printf.printf "%b\n" (Int64.equal before after)

let test =
  test_rdtsc ();
  test_rdpmc ();
  ()

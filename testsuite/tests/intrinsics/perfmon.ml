(* TEST
 * bytecode
 * native
*)

(* how to enable it only for amd64? *)

external perfmon : string -> int64 -> int64  = "%perfmon"

let rdtsc () = perfmon "rdtsc" 0n
let rmpmc c = perfmon "rdpmc"

let[@inline never] opaque x = Sys.opaque_identity x

let work () =
  let min = 0, max = 100 in
  let e = ref 0L in
  for i = min to max do
    e :=  !e + Int64.to_int i
  done;
  opaque !e

let test_rdtsc () =
  let before = rdtsc () in begin
    work ();
    let after = rdtsc () in
    (* assert before <> after *)
  end;
  ()

let test_rdtsc () =
  let c = 0L in
  let before = rdpmc c in begin
    work ();
    let after = rdpmc c in
    (* assert before <> after *)
  end;
  ()

let test =
  test_rdtsc ();
  test_rdpmc ();
  ()

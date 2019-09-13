(* TEST
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
   flags = "-stop-after scheduling -S"
   ocamlopt_byte_exit_status = "0"
*** script
   script = "sh ${test_source_directory}/stop_after_scheduling.sh"
**** check-ocamlopt.byte-output
*)

(* this file is just a test driver, the test does not contain real OCaml code *)

(* TEST
 * native-compiler
 ** setup-ocamlopt.byte-build-env
 *** ocamlopt.byte
   flags = "-save-ir-after linearize -S"
   ocamlopt_byte_exit_status = "0"
 **** check-ocamlopt.byte-output
 ***** script
   script = "sh ${test_source_directory}/save_ir_after_linearize.sh"
*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y

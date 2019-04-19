(* Transformation is identity function by default *)
let transform = ref (fun fundecl -> fundecl)

(* Change transformation *)
let setup ~f =  transform := f

(* Apply transformation *)
let fundecl f = !transform f

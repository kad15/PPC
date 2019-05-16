open Facile
open Easy

let storage = fun vars ->
  let allsol = ref [] in
  let store =
    Goals.atomic
      (fun () ->
        let sol = Array.map Fd.elt_value vars in
        allsol := sol :: !allsol) in
  let get = fun () -> List.rev !allsol in
  (store, get) (* store = but, get = fct*)

    (* int array printer *)
let fprint_sol = fun ch sol ->
  Printf.fprintf ch "[%d" sol.(0);
  for i = 1 to Array.length sol-1 do
    Printf.fprintf ch ";%d" sol.(i) done;
  Printf.fprintf ch "]"
     

  
let solve = fun n ->
  let vars = Fd.array n 1 n in
  let (store, get_allsol) = storage vars in
  Cstr.post (Alldiff.cstr vars);
  let goal = Goals.Array.labeling vars in
ignore (Goals.solve (goal &&~ store &&~ Goals.fail));

List.iter
  (fun sol -> Printf.printf "%a\n" fprint_sol sol)
  (get_allsol ())

  let () = solve (int_of_string Sys.argv.(1))

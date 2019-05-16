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
     


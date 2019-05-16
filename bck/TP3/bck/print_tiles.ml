let fprint = fun filename t x y -> (* printing for GNUplot *)
  let ch = open_out filename in
  Array.iteri
    (fun i ti ->
      let xi = Fd.elt_value x.(i) and yi = Fd.elt_value y.(i) in
      Printf.fprintf ch "%d %d\n%d %d\n%d %d\n%d %d\n%d %d\n\n"
        xi yi (xi+ti) yi (xi+ti) (yi+ti) xi (yi+ti) xi yi)
    t;
  close_out ch


let read = fun file ->
  let chin = open_in file in
  let (m, n) = Scanf.fscanf chin " %d %d " (fun m n -> (m, n)) in
  let flights = Array.make_matrix m n 0 in
  let costs = Array.make n 0 in
  for j = 0 to n-1 do
    let (cj, nb1) =
      Scanf.fscanf chin " %d %d %d " (fun cj nb1 _ -> (cj, nb1)) in
    costs.(j) <- cj;
    for k = 0 to nb1-1 do
      let i = Scanf.fscanf chin " %d " (fun i -> i) in
      flights.(i-1).(j) <- 1
    done
  done;
  (costs, flights)

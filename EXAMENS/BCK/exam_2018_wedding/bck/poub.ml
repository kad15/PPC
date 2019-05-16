(*let _ = Array.iter (fun i ->  Fd.fprint stdout tab.(i)) tab*)
(*
for i = 0 to m-1 do
  begin
  for j = 0 to n-1 do
 printf "%a " Fd.fprint table.(i).(j) 
  done;
    print_newline()
 end
done
 *)


for i = 0 to m-1 do
  Cstr.post(Arith.sum_fd tab =~ i2e k)
done

  let col = Array.make m 0 in (* col de la matrice table *)

 for i = 0 to m-1 do
        col.(i) <- table.(i).(j) 
     done;



Fd.fprint_array stdout table.(0)


 Cstr.fprint stdout (Arith.sum_fd table.(3) =~ i2e k) 




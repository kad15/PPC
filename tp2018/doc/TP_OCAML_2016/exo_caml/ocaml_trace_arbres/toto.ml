let l = ref [];;
let find_address name = List.assoc name !l;;
let add_address name address = l := (name, address) :: ! l;;
add_address "IRIA" "Rocquencourt";;
print_string (find_address "INRIA"); print_newline ();;

open Facile
open Easy

let [|x;y|] as vars = Fd.array 2 1 3;;
let nogoods = [(1,1);(1,3);(2,1);(2,2);(2,3);(3,1)];;
Cstr.post (Binary.cstr x y nogoods);;
Fd.fprint_array stdout vars;;

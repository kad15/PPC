(* 1 
Les expressions arithm ́etiques consid ́er ́ees peuvent comporter les termes suivants :
— des constantes enti`eres ;
— des variables, nomm ́ees par leur identificateur (chaˆıne de caract`eres) ;
— un tableau nomm ́e par son identificateur et index ́e par une expression (le premier  ́el ́ement
du tableau  ́etant index ́e par 0) ;
— la somme de deux expressions ;
— le produit de deux expression
*)


type expr =
    Cst of int
  | Var of string
  | T of expr * expr
  | Sum of expr * expr
  | Prod of expr * expr
;;
(*2
[1pt] Repr ́esenter l’expression 1 donn ́ee en exemple dans l’introduction `a l’aide du type
expr.
*)

let e= Sum (Prod (Cst 5, Sum (Var "x", Cst (-3))), T (Var "t", Sum (Var "y", Cst 1)));;


(*3 
3. [3pt] 
 ́
Ecrire
la fonction adresse: (string * int) array -> string -> int qui prend en pa-
ram`etres l’environnement et un identificateur, et renvoie l’adresse m ́emoire associ ́ee. On veillera a ne pas parcourir de cases inutilement et a lever une exception si l’identificateur
n’existe pas dans l’environnement.
# adresse env "t";;
- : int = 2
*)

let rec adresse env id =
  let n = Array.length env in 
  let k = ref 0 in  
  while fst(env.(!k)) = id do
    incr k
  done;
  if !k < n then snd(env.(!k)) else failwith "id not found in env!" ;;



(* ́
4. [4pt] Ecrire
la fonction eval: (string * int) array -> int array -> expr -> int
qui prend en param`etres une expression, un environnement et un tableau m ́emoire, et
renvoie la valeur de l’expression.
# eval env memory e;;
- : int = 42
*)

let mem = [|11;2;5;4;3;2;1;|];;
let env = [|("x",0);("y",1);("t",2)|];;
let t = [|5;4;3;2;1|];;

let eval env mem exp =
  let rec eval_rec e =
  match e with
    Cst c -> c
  | Var id ->  mem.(adress env id)
  | T (e1,e2) ->  mem.(adress env e1) + eval_rec env mem e2
  | Sum (e1, e2) -> eval_rec e1 + eval_rec e2 
  | Prod (e1, e2) -> eval_rec e1 * eval_rec e2 
  in eval_rec exp
;;

(*5*)

let rec iter f exp = 
  






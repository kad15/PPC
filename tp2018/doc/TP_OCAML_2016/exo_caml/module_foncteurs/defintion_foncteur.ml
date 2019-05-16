(*
Un foncteur est un module qui est paramétré par un autre module, tout comme une fonction n'est qu'une valeur paramétrée par d'autre valeurs (les arguments). En gros, cela permet de paramétrer un type par une valeur, ce qui est impossible à faire directment en OCaml. Par exemple, nous pourrions définir un foncteur prenant un entier n et retournant un ensemble d'opérations sur des tableaux de longeurs n uniquement. Si par erreur, un programmeur donne un tableau normal à une de ces fonctions, le compilateur soulevera une erreur. Si nous n'utilisions pas un foncteur mais le type standard des tableaux, le compilateur ne sera pas capable de détecter l'erreur, et nous obtiendrions une erreur à l'execution bien après la compilation ce qui est bien pire !


*)

Comment définir un foncteur ?

Un foncteur avec un argument peut être définit comme ceci:

module F (X : X_type) =
struct
  ...
end
où X est le module passé en argument, et X_type est sa signature, qui est obligatoire.

La signature du module obtenu peut elle même être restreinte à l'aide la syntaxe habituelle:
où Y_type est la signature d'un module Y qui définit les objet utilisable

(* syntaxe habituelle voir ci-dessous Hello : Hello_type où Hello est un module "normal"
et pas un functor !!!

module type Hello_type =
sig
  val hello : unit -> unit
end
  
module Hello : Hello_type =
struct
  ...
end  
 *)


module F (X : X_type) : Y_type =
struct
  ...
end

ou bien en le spécifiant dans le fichier .mli:

module F (X : X_type) : Y_type

La syntaxe des foncteurs reste cependant difficile à assimiler. Il est donc préférable de jeter un coup d'oeil aux fichier sources set.ml ou map.ml dans la librairie standard. Une dernière remarque: les foncteurs ont été conçus pour aider les programmeurs et non pas pour améliorer les performances. L'execution est même plus lente, à moins d'utiliser un défoncteur comme ocamldefun, qui requiert un accès au code source du foncteur.

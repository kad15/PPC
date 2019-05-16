(*
Insertion dans un module

Supposons que nous sentions qu'une fonction manque au module standard List et que nous désirions qu'elle en fasse partie intégrante. Il est possible d'utiliser l'instruction include au sein d'un fichier extensions.ml afin d'insérer notre fonction:
*)

module List =
struct
  include List
  let rec optmap f = function
    | [] -> []
    | hd :: tl ->
       match f hd with
       | None -> optmap f tl
       | Some x -> x :: optmap f tl
end
(*
Nous avons créé un nouveau module Extensions.List contenant , en plus des fonctions habituelles du module List, une nouvelle fonction optmap. Depuis un autre fichier, il nous suffit d'ouvrir notre module Extensions pour que celui-ci « écrase » le module List standard :
*)

open Extensions
...
List.optmap ...

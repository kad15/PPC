open Odot

let node lbl name =
  Stmt_node 
    ((dblq_node_id lbl), [(Simple_id "label", 
			   Some (Double_quoted_id name))])

let edge id1 id2 = Stmt_edge
  ((Edge_node_id (dblq_node_id id1)),
   [Edge_node_id (dblq_node_id id2)], [])

let draw_graphdot name = 
  let stmt_list = [node "0" "0"; 
	      node "1" "0"; node "2" "2";
	      node "3" "0" ; node "4" "3";
	      node "5" "2" ; node "6" "5";
              node "7" "0" ; node "8" "4";
	      node "9" "3" ; node "10" "7";
              node "11" "2" ; node "12" "6";
	      node "13" "5" ; node "14" "9";
              node "15" "0" ; node "16" "6";
	      node "17" "4" ; node "18" "10";
              node "19" "3" ; node "20" "9";
	      node "21" "7" ; node "22" "13";
              node "23" "2" ; node "24" "8";
	      node "25" "6" ; node "26" "12";
              node "27" "5" ; node "28" "11";
	      node "29" "9" ; node "30" "15";

	      edge "0" "1" ; edge "0" "2" ;
	      edge "1" "3" ; edge "1" "4";
	      edge "2" "5"  ; edge "2" "6";
              edge "3" "7" ; edge "3" "8" ;
	      edge "4" "9" ; edge "4" "10";
	      edge "5" "11"  ; edge "5" "12";
              edge "6" "13"  ; edge "6" "14";
              edge "7" "15" ; edge "7" "16" ;
	      edge "8" "17" ; edge "8" "18";
	      edge "9" "19"  ; edge "9" "20";
              edge "10" "21" ; edge "10" "22" ;
	      edge "11" "23" ; edge "11" "24";
	      edge "12" "25"  ; edge "12" "26";
              edge "13" "27"  ; edge "13" "28";
	      edge "14" "29"; edge "14" "30"]
  in
  {strict=true;kind=Digraph; stmt_list ;
   id = Some (Double_quoted_id name)}
   
let () =   
  let graph = draw_graphdot "Graph" in
  Odot.print_file "arbre.dot" graph;
  ignore (Sys.command "dot -Tsvg arbre.dot -o arbre.svg");
  ignore (Sys.command "dot -Tpng arbre.dot -o arbre.png")

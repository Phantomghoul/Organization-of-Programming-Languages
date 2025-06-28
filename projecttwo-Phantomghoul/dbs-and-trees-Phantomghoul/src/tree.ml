type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

(*return type ('a -> 'b -> 'a -> 'a) -> 'a -> 'b tree -> 'a *)
let rec tree_fold f init tree =
  match tree with
  | Leaf -> init
  | Node (left, value, right) ->
      (*this returns left/ returns hello*)
      let left_tree = tree_fold f init left in
      (*returns !*)
      let right_tree = tree_fold f init right in 
      (*does left + value + right // hello ^ world ^ !*)
      f left_tree value right_tree;;

(*must use tree_fold*)
(*return type 'a tree -> ('a -> 'b) -> 'b tree *)
(*first thought is, implementing a map is pretty simple, just recursively call f on each node*)
(*Translating into a fold is quite tough*)
(*figured it out in an hour, f is going to create a node (leaf, f value, leaf) and the base case is when the tree is a leaf*)
(*accumulator should be a leaf*)
let map tree f =
  tree_fold (fun left_tree value right_tree -> Node (left_tree, f value,right_tree)) Leaf tree;;

(*just reverse the order*)
let mirror tree = 
  tree_fold(fun left_tree value right_tree -> Node(right_tree, value, left_tree)) Leaf tree;;

(*left tree then right three*)
(*returns a tree but adding the values to a list, using map*)
(*correction, it returns a list *)
(*first attempt, used a map*)
(*second attempt, use tree_fold to accumulate instead*)
let in_order tree = 
  (*left tree val left tree*)
  (*Took two hours, used lecture notes example*)
  tree_fold(fun left_tree value right_tree -> left_tree@[value]@right_tree) [] tree;;

(*same as in_order but value @ left @ right*)
let pre_order tree = 
  tree_fold(fun left_tree value right_tree -> [value]@left_tree@right_tree) [] tree;;


(*return type ('a -> 'a)tree -> 'a -> 'a*)
(*took an hour, this one is stupid*)
let compose tree = 
  (*each node will have a function*) 
  tree_fold(fun left_tree value right_tree -> fun x -> right_tree (value (left_tree x))) (fun x -> x) tree;;
  
(*return type 'a tree -> int*)
(*10 minutes*)
let depth tree = 
  tree_fold (fun left_tree value right_tree -> 1 + max (left_tree) (right_tree)) 0 tree;;

(* Assume complete tree *)
(* Assume complete tree *)
(*return type 'a tree -> int -> 'a tree*)
(* 1-2 hrs*)
(*first method try to pattern match*)
let trim tree n = 
  (*second attempt use a helper*)
  (*for a perfect binary tree, calculate depth starting from node, if level isn't n then add 1 to left and right tree*)
  let helper_trim = tree_fold (fun left_tree value right_tree -> (fun d -> if d = n then Node (Leaf, value, Leaf)
                                                                   else Node (left_tree (d + 1), value, right_tree (d + 1))) ) (fun x -> Leaf) tree 
  in helper_trim 1;;

(* return type 'a -> ('a * 'b * 'a )option)-> 'a -> 'b tree*)
(*similar to compose tree but generates a tree*)
(*30 minutes*)
let rec tree_init f v =
  match f v with
  | None -> Leaf
(*three parameters, left_tree +1, val, right_tree+1 *)
  | Some (left_tree, value, right_tree) -> Node (tree_init f left_tree, value, tree_init f right_tree);;

(*return type 'a list -> 'a -> 'a list * 'a list*)
let rec split lst v =
  match lst with
  | [] -> ([], [])
(*keep pushing elements in the first list until a == v*)
  | (a::b) -> if a = v then ([], b)
      else let (x, y) = split b v 
        in (a::x, y);;


let rec from_pre_in pre in_ord = failwith "unimplemented"
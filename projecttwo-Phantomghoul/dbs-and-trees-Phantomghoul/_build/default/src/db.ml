type person = { name : string; age : int; hobbies : string list }
type comparator = person -> person -> int
type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

(* TODO: Implement functions below *)

type db = {database : person list};;

let newDatabase = {database = []};;

let insert person db = {database = person::db.database};;

let remove name db =
  let rec helper_del name database lst = match database with
    | [] -> lst
    | (a::b) -> 
        if a.name = name then helper_del name b lst 
        else helper_del name b (a::lst) 
  in { database = helper_del name db.database [] };;

let sort comparator db = List.sort comparator db.database;;

let rec func_cond p cond  =
  match (p,cond) with 
  | (_,True) -> true
  | (_,False) -> false
  | (p, Age x) -> x p.age
  | (p, Name x) -> x p.name
  |(p, Hobbies x) -> x p.hobbies
  |(p, And (x,y)) -> func_cond p x && func_cond p y
  |(p, Or (x,y)) -> func_cond p x || func_cond p y
  |(p, Not x) -> not (func_cond p x)
  |(p, If (a,b,c)) -> if func_cond p a then func_cond p b else func_cond p c;;

(*First thought, create a record with and a field of database, insert elements with condition, do it recursively*)

(*RETURN TYPE condition -> db -> comparator -> person list*)
let query condition db = 
  (*will first try using recursion to insert each element
  logic is if condition is true, insert, else don't*)
  let rec helper_con db lst condition = match db with
    (*[] -> lst returns db*)
    |[] -> lst
    |(a::b) -> 
        (*stuck on how to check if a condition matches a person*)
        (*a = condition is true?*)
        (* && operator checks if a and b are true*)
        (*a && condition; compiler expects a to be a bool statement*)
        (*a = condition; expects condition to be a person*)
        (*use an exhaustive function*)
        if func_cond a condition then helper_con b (a::lst) condition
        else helper_con b lst condition
(*create a new database
  {database = helper_con db.database [] condition returns db instead of person list
  return just the helper function instead}
*)
  in  helper_con db.database [] condition;;

(*return type condition -> db -> comparator -> person list*)
(* sort them (return the list of persons who satisfy the condition)*)
(*make sure the sort parameter is a database *)
let queryBy condition db comparator = sort comparator {database = query condition db};;

(*return type condition -> db -> (person ->person) -> db*)
(*believe this function is asking me to update every element that satisfies the condition given with the function*)
let update condition db change = 
  let rec helper_upd condition db change lst = match db with
    |[] -> List.rev lst
    |(a::b) -> if func_cond a condition then helper_upd condition b change ((change a)::lst)
        else helper_upd condition b change (a::lst) 
(*create a new database*)
  in {database = helper_upd condition db.database change []};;


(*return type condition -> db -> db*)
(*gotten the hang of it, no need to comment anymore*)
let deleteAll condition db = 
  let rec helper_del condition db lst = match db with
    | [] -> lst
    | (a::b) -> if func_cond a condition then helper_del condition b lst
        else helper_del condition b (a::lst)
  in {database = helper_del condition db.database []};;

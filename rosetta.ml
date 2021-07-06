(* 
   Tyler Laudenslager

   I learned pattern matching, recursion,
   higher-order functions, and dictionaries 
   from the book

   "OCaml from the very beginning"
   by John Whitington

   I learned how the merge sort could be implemented in OCaml
   in the same book mentioned above on page 39.
   
   I learned about anonymous functions and the optimization
   provided from tail recursive functions from
   https://cs.cornell.edu/courses/cs3110/2020fa/textbook/
   
*)


(* 
    appends a key and a value to a
    dictionary if the key exists
    just append the value instead
    
    input - k - key to add
    input - v - value to associate with key
    input - d - the dictionary to add the
                values to


    returns - a dictionary with the added
              value / key

*)


let rec append_to_dict = fun k v d ->
    match d with
     [] -> (k, [v])::[]
   | (k', v')::t ->
     if k = k'
     then (k, v::v')::t
     else (k', v')::append_to_dict k v t


(* replace a key value pair in a dictionary
   
   input k - key to replace
   input v - value to replace with the key
   input d - dictionary to use
   
   returns - dictionary with the appended
             key value pairs
*)


let rec append_list = fun k v d ->
    match d with
      [] -> (k, v)::[]
    | (k', v')::t ->
      if k = k'
      then (k, v)::t
      else (k', v')::append_list k v t

(* add a value to the front of a list
   
   input - x - value to add to list
   input - l - the list to add the value
               to
   return - the list with the appended
            value
*)

let add_to_front_of_list = fun x l ->
    match l with
     [] -> [x]
    |h::t -> x::h::t

(* searches for a value in a list
   
   input - x - value to search for
   input - l - list to search in

   return - true if x in the list else false
*)

let rec member = fun x l ->
  match l with
  [] -> false
 |h::t -> if h = x then true else member x t

(* access the value that a key in a dictionary
   refers to

   input - k - the key that is associated with
               the value that is returned
   input - d - the dictionary that contains the
               key

   returns - the value that the key associates with

*)


let rec access = fun k d ->
    match d with
     [] -> []
    |(k', v')::t -> if k = k'
                    then v'
                    else access k t

(* add a value to the end of a list
   
   input - x - value to add to the list
   input - l - the list to append the value
               to
   returns - a list with the value added
             to the end
*)

let rec add_to_end_of_list = fun x l ->
   match l with
    [] -> [x]
   | h::t -> h::add_to_end_of_list x t

(* remove all duplicates from a list
   
   input x - a list that contains duplicates
   
   returns - a list without duplicates
*)

let rec make_set = fun x ->
    match x with
    [] -> []
  | h::t -> if member h t then make_set t else h::make_set t


(* 
   drops the first n values of a' list
   
   input n : number of values to drop
   input l : 'a list to drop the values from
  
   returns : 'a list with the first n values dropped
              from l
*)

let rec drop = fun n l ->
  match l with
   [] -> []
  | h::t -> if n = 0 then h::t else drop (n-1) t




(* take the first n values of 'a list
   input n : number of values to take
   input l : 'a list to take the values from

   returns : 'a list with the first n values taken
             from l
*)

let rec take = fun n l ->
  match l with 
    [] -> []
  | h::t -> if n = 0 then [] else h::take (n-1) t

(* remove the first value from the list and return
   the value added to a empty list
   
   input - list to remove the first value from
   returns - first value in a empty list
*)


let pop = fun l -> take 1 l, drop 1 l

(* tail recursive helper function that
   returns how many elements are in an
   unknown list type
      x : 'a list
      l : int
*)

let rec length_helper = fun x l ->
    match x with
     [] -> l
    | h::t -> length_helper t (l+1)



(* tail recursive function wrapper
   abstracts length_helper function 
   so we make sure it works properly
   providing the initial proper number
   of 0  
   input x : 'a list
   returns length 'a list : int
*)

let length = fun x -> length_helper x 0




(* combines two sorted lists into one sorted list
   this function "zips" together two lists into
   list that is sorted. This works because we
   compare the first two elements and have
   two branches depending on which element
   needs to come first in the list to
   be sorted.

    input x : 'a list that is sorted
    input y : 'a list that is sorted

    returns : 'a list that is x merged with y
*)
 
let rec merge = fun cmp x y ->

   match x, y with
   
   [], l -> l
  |l , [] -> l
  | hx::tx, hy::ty ->
    
    if cmp hx hy
      then hx::merge cmp tx (hy::ty)
      else hy::merge cmp (hx::tx) ty



(* basic merge sort : sorts 'a list
   using the merge sort algorithm.
   The list gets divided into smaller
   and smaller lists until we have
   just one item and then we merge
   lists recursively until we have
   a sorted list.
   
   input x : 'a list that needs to be sorted
   returns : 'a list that is sorted
*)

let rec msort = fun cmp x ->
   match x with
     [] -> []
   | [x] -> [x]
   | _ ->
      let half = length x / 2 in
      let left = take half x in
        let right = drop half x in
          merge cmp (msort cmp left) (msort cmp right)

(* I adapted the code examples from this site to match
   what I wanted to accomplish in the next two functions

   https://riptutorial.com/ocaml/example/9450/
   read-from-standard-input-and-print-to-standard-output
*)

(* returns line from standard in unless end of file has
   been reached then return None. This function uses the
   functionality of options when there are no appropriate
   values to pass back to the caller

   input - NA
   return - line string from standard in or None

*)

let read_line_opt() =
    try Some(read_line())
    with End_of_file -> None

(* read a file which has task dependent pairs meaning that the task
   depends on the dependent task to execute first in processing the
   task and return a adjaceny list with the keys as the tasks and
   the values as the tasks that have to be executed first before the
   key is allowed to be processed. If you have multiple files to process
   you can call this function multiple times the output will go into
   the next call. Normally the function takes a empty list [] to start.

   input - adjaceny_list or [] to make an adjaceny_list
   return - adjaceny_list made from file of task dependent pairs
*)
let rec get_input = fun acc ->
    match read_line_opt (), read_line_opt () with
    Some(task), Some(dependent) -> get_input (append_to_dict task dependent acc)
   |_,_ -> acc

(* helper function for list_to_string
   formats the list so that every value
   in the list is seperated by a endline
   character
*)
let rec print_list_h = fun l ->
    match l with
    [] -> ""
   |h::[] -> h ^ print_list_h []
   |h::t -> h ^ "\n" ^ print_list_h t

(* converts the contents of a list to a string
   that can be passed to print_string function
   with a endline character at the end
*)
let list_to_string = fun l -> print_list_h l ^ "\n"

(* removes a value from a list 
   input -
   x - value to remove
   v - list that contains the value
   
   returns - the same list not including the removed value

*)

let rec remove = fun x v ->
    match v with
     [] -> []
    |h::t -> if x = h then remove x t else h::remove x t

(* returns a list of all the keys in the dictionary
    
    input - a dictionary  ( a' * b') list
    returns - the keys of the dictionary (a' list)
*)

let rec keys = fun d ->
   match d with
    [] -> []
  | (k', v')::t -> k'::keys t

(* checks to see if the value passed in is a member
   of the keys list

   input - v - the value to search for
   input - k - the keys list to search in

   return - true if found else false
*)

let rec value_in_keys = fun v k ->
    match k with
    [] -> false
   |h::t -> if v = h then true else value_in_keys v t

(* obtain all the nodes in the graph that have no incoming
   edges
   
   input - v - list of tasks that need to be processed
               before we can process the task that refers
               to this list

   input - l - list of keys that are in the dictionary
*)

let rec get_dependents = fun v l ->
    match v with
     [] -> []
   | h::t -> if value_in_keys h l then get_dependents t l else h::get_dependents t l

(* this function searches through all lists that are
   associated with keys to find all nodes with no
   incoming edges

   input - d - adjaceny_list
   input - keys - list of keys that are
                  in the dictionary

   return - a list of all the nodes
            in the graph without
            a incoming edge
*)

let rec no_dependents_h = fun d keys ->
   match d with
    [] -> []
  | (k', v')::u ->
   match v' with
    [] -> []
  | h::t -> (get_dependents (h::t) (keys))@no_dependents_h u keys

(* return the first element of a list
   of strings 
   
   input - a list of strings
   returns - first string of the list
*)

let get_front = fun l ->
    match l with
    [] -> ""
   |h::t -> h

(* obtain a list of all the keys
   that have the next node that
   has no incoming edges so
   we can remove the node
   from each of the keys
   in the dictionary

   input - nn - node that has
                already been
                added to the final
                task list
   input - k - list of keys in the
               dictionary d
    
   input - d - the dictionary that
               has the keys k

   returns - a list of all the keys
             that associate nn as
             a value
*)

let rec dependents = fun nn k d ->
    match k with
     [] -> []
    | h::t -> if member nn (access h d) then h::dependents nn t d
     else dependents nn t d

(* check all the keys that associated
   the new node and if the key is
   associated with a empty list then
   we can add the key to the final
   task queue because it has no other
   tasks that it depends on. However
   other tasks might depend on the
   key so we add it to the no dependents
   list instead of the task queue.

   input key - a key in the dictionary d
   input l - no_dependents list
*)  

let check_if_add_to_list = fun k l d -> if length (access k d) = 0 then add_to_front_of_list k l else l

(* check the adjaceny list to see
   if all the lists are of length
   0 if not there is a logic cycle
   in the process file

   input - adjaceny_list
  
   returns true if cycle else false
*)

let rec check_adj_list = fun adj_list ->
      match adj_list with
        [] -> false
      | (k', v')::t -> if length v' = 0 then check_adj_list t else true

(* remove a value from a dictionary using a key
    simply : dict[key].remove(v)
    
    input k - key that is in d
    input v - value to remove
    input d - dictionary that has k
              as a key

    returns - a new dictionary of
              the same type with
              the value removed
              from the proper key

*)

let remove_value_d = fun k v d -> append_list k (remove v (access k d)) d


(* get all the starting nodes that do not
   have any incoming edges in the graph

   input - d - adjaceny_list
   returns - a set of sorted nodes
             in a list
*)

let sort_start_dependents = fun d -> make_set (msort ( <= ) (no_dependents_h d (keys d)))

(* for every key that has new node as a value
   we remove the node from the list that the key
   is paired with and we see if the key
   has anymore tasks it depends on if it does not
   we add it to the no dependents list
   
   dp_list - reference to all the keys that include new_node as a value
   new_node - reference to the node that has just been added to
              the final task_queue
   adj_list - reference to the adjaceny list
   no_depend_list - reference to a list of all the tasks that have no
                    dependent tasks

   returns - the updated adjaceny_list
*)  

let rec dependents_loop = fun dp_list new_node adj_list no_depend_list ->
        match dp_list with
            [] -> !adj_list
           | h::t -> adj_list := remove_value_d h !new_node !adj_list;
                     no_depend_list := check_if_add_to_list h !no_depend_list !adj_list;
                     dependents_loop t new_node adj_list no_depend_list

(* figures out which order the tasks in the adjaceny list
   should be processed so that every task that is executed
   does not have to wait for other tasks to be processed.
   
   input - task_q - reference to the final order of the tasks
                    that are in a topological order
   input - adj_list - reference to a adjaceny_list
   
   input - new_node - reference to the node that was just added
                      to the task queue (it starts out as a empty string)
   
   input - no_depend_list - reference to a list that contains all the tasks
                            that do not depend on other tasks to be processed.
   
   input - new_dependents - reference to a list that contains all the tasks
                            that depend on the new_node to be processed
                            before they can be processed

   returns - ["empty"] if there is a loop else returns the final
             topological ordering of the tasks in the adjaceny list
             inside a list.

*)

let rec topo_sort = fun task_q adj_list new_node no_depend_list new_dependents ->
      match !no_depend_list with
        [] -> if check_adj_list !adj_list then ["cycle"] else !task_q
      | h::t -> no_depend_list := msort ( <= ) !no_depend_list;
                let node, _ = pop !no_depend_list in
                let _, no_list = pop !no_depend_list in
                new_node := get_front node;
                no_depend_list := no_list;
                task_q := add_to_end_of_list !new_node !task_q;
                new_dependents := dependents h (keys !adj_list) !adj_list;
		adj_list := dependents_loop !new_dependents new_node adj_list no_depend_list;
                topo_sort task_q adj_list new_node no_depend_list new_dependents


let main () = let task_queue = ref ([]) in
              let adj_list = ref (get_input []) in
              let new_node = ref ("") in
              let no_dependents = ref (sort_start_dependents !adj_list) in
              let new_dependents = ref [] in
              let new_task_queue = topo_sort task_queue adj_list new_node no_dependents new_dependents in
              print_string ( list_to_string new_task_queue)

let () = main ()


                
                
                
 



(* List.for_all checks if all elements of the a satisfy the predicate.
   List.mem is true if and only if i is equal to an element of b. *)
let subset a b =
	List.for_all (fun i -> List.mem i b) a;;

(* If two sets are subset of each other, then they must be equal. *)
let equal_sets a b =
	(subset a b) && (subset b a);;

let rec clean_repeat a =
	match a with 
	| [] -> []
	| h::t -> if List.mem h t then clean_repeat t else h::(clean_repeat t);;
	
(* We first concatenate b to a. Then we delete duplicates from them*)
let set_union a b = 
	clean_repeat (a@b);;

let rec set_intersection a b =
	match a with
	| h::t -> if (List.mem h b) then [h] @ (set_intersection t b) else set_intersection t b
	| [] -> [];;

let rec set_diff a b =
	match a with
	| h::t -> if not (List.mem h b) then [h] @ (set_diff t b) else set_diff t b
	| [] -> [];;

(* It will go into loop if there is no computed fixed point. *)
let rec computed_fixed_point eq f x = 
	let res = f x in
	if (eq res x) then x else computed_fixed_point eq f res;;

(* Calculate the value after p period *)
let rec periodic_check f p x = 
	match p with
	| 0 -> x
	| _ -> periodic_check f (p-1) (f x);;

(* Try points for a periodic point *)
let rec computed_periodic_point eq f p x = 
	let res = periodic_check f p x in
	if eq res x then x else computed_periodic_point eq f p (f x);;

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal;;
  
let rec in_terminal_list s terminal_list = 
	match terminal_list with
	| [] -> false
	| (l, r)::t -> if l = s then true else in_terminal_list s t;;

(* Check if the right hand side of s in s_list exists in the terminal list, or if it is a terminal. *)
let rec terminal_rule_check s_list terminal_list =
	match s_list with
	| [] -> true
	| (T _)::t -> terminal_rule_check t terminal_list (* If terminal, skip it *)
	| (N s)::t -> if in_terminal_list s terminal_list then terminal_rule_check t terminal_list else false;; (* If nonterminal, check if it exists in terminal_list *)

(* Scan the rules list once, and add any terminable rules to the terminal_list *)
let rec add_to_terminal_list rules terminal_list =
	match rules with
	| [] -> terminal_list
	| rule::t -> if terminal_rule_check (snd rule) terminal_list then add_to_terminal_list t (terminal_list@[rule])
													else add_to_terminal_list t terminal_list;;

(* Pick each rule in rules and terminal_list, and add it to the final result. 
   The main reason doing this is that the filtered rules should be in the same
   order as they were in the original list. *)
let rec pick_non_blind_alley_rules terminal_list rules =
	match rules with
	| [] -> []
	| h::t -> if List.mem h terminal_list then h::(pick_non_blind_alley_rules terminal_list t)
										  else pick_non_blind_alley_rules terminal_list t;;

let filter_blind_alleys g =
	match g with
	| (l, rules) -> l, clean_repeat (pick_non_blind_alley_rules (computed_fixed_point equal_sets (add_to_terminal_list rules) []) rules);;

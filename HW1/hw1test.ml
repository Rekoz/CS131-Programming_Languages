let my_subset_test0 = subset [1;1;1] [1;2;3];;
let my_subset_test1 = subset [] [];;

let my_equal_sets_test0 = equal_sets [] [];;
let my_equal_sets_test1 = not (equal_sets [3;1;3] [1;2;3]);;

let my_set_union_test0 = equal_sets (set_union [3;3;3;3] [4;4;4;4]) [3;4];;
let my_set_union_test1 = not (equal_sets (set_union [3;3;3;3] [4;4;4;4]) [4;4]);;
let my_set_union_test2 = not (equal_sets (set_union [3;3;3;3] [4;4;4;4]) [4]);;

let my_set_intersection_test0 =
  equal_sets (set_intersection [] []) [];;
let my_set_intersection_test1 =
  not (equal_sets (set_intersection [5;6;3;8] [1;3;7;8;9]) [3]);;
let my_set_intersection_test2 =
  equal_sets (set_intersection [5;6;3;8] [1;3;7;8;9]) [3;8];;

let my_set_diff_test0 = equal_sets (set_diff [1;1;1;1;] [4;3;1]) [];;
let my_set_diff_test1 = equal_sets (set_diff [1;1;1;1;2] [4;3;1]) [2];;

let my_computed_fixed_point_test0 =
  computed_fixed_point (<) sqrt 10. = 10.;;

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x *. x -. 1.) 4 0.5 = -1.;;
let my_computed_periodic_point_test1 =
  computed_periodic_point (<) (fun x -> x / 2) 4 1 = 1;;
  
type my_nonterminals =
  | S | V | O | A | B | Sub | Verb | Obj ;;

let my_grammar =
  S,
  [S, [N Sub; N V; N O];
   S, [N Sub; N S];
   V, [N Verb];
   V, [N Verb];
   O, [N Obj];
   A, [N Sub; N B; N O];
   B, [N B];
   B, [N A];
   Sub, [T "I"];
   Sub, [T "You"];
   Sub, [T "He"];
   Sub, [T "She"];
   Sub, [T "It"];
   Verb, [T "love"];
   Verb, [T "hate"];
   Verb, [T "think"];
   Obj, [T "apples"];
   Obj, [T "bananas"];
   Obj, [T "oranges"];
   Obj, [T "that"; N S]];;
   
let my_filter_blind_alleys_test0 = 
	filter_blind_alleys my_grammar = 
	(S,
	[S, [N Sub; N V; N O];
	S, [N Sub; N S];
	V, [N Verb];
	O, [N Obj];
	Sub, [T "I"];
	Sub, [T "You"];
	Sub, [T "He"];
	Sub, [T "She"];
	Sub, [T "It"];
	Verb, [T "love"];
	Verb, [T "hate"];
	Verb, [T "think"];
	Obj, [T "apples"];
	Obj, [T "bananas"];
	Obj, [T "oranges"];
	Obj, [T "that"; N S]]);;
	
let my_filter_blind_alleys_test1 = 
	filter_blind_alleys (S, List.tl (snd my_grammar)) = 
	(S,
	[V, [N Verb];
	O, [N Obj];
	Sub, [T "I"];
	Sub, [T "You"];
	Sub, [T "He"];
	Sub, [T "She"];
	Sub, [T "It"];
	Verb, [T "love"];
	Verb, [T "hate"];
	Verb, [T "think"];
	Obj, [T "apples"];
	Obj, [T "bananas"];
	Obj, [T "oranges"]]);;
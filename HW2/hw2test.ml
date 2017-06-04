type my_nonterminals =
  | S | V | O | A | B | Sub | Verb | Obj ;;

let my_grammar =
  S,
  [S, [N Sub; N V; N O];
   S, [N Sub; N V];
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
   Obj, [T "that"; N S]];;
   
let converted_grammar = convert_grammar my_grammar;;

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None
   
let rec contains_obj = function
   | [] -> false
   | (Obj, _)::_ -> true
   | _::rules -> contains_obj rules;;
   
let accept_only_sub_and_verb derivation frag = 
   if contains_obj derivation
   then None
   else Some (derivation, frag);;

let test_1 = 
  ((parse_prefix converted_grammar accept_all ["I"; "think"; "that"; "I"; "love"; "oranges"; "He";])
   = Some
       ([(S, [N Sub; N V; N O]); (Sub, [T "I"]); (V, [N Verb]);
	 (Verb, [T "think"]); (O, [N Obj]); (Obj, [T "that"; N S]);
	 (S, [N Sub; N V; N O]); (Sub, [T "I"]); (V, [N Verb]);
	 (Verb, [T "love"]); (O, [N Obj]); (Obj, [T "oranges"])],
	["He"]));;
	
let test_2 = 
  ((parse_prefix converted_grammar accept_only_sub_and_verb ["I"; "think"; "that"; "I"; "love"; "oranges"; "He"])
   = Some
       ([(S, [N Sub; N V]); (Sub, [T "I"]); (V, [N Verb]);
	 (Verb, [T "think"])],
	["that"; "I"; "love"; "oranges"; "He"]));;
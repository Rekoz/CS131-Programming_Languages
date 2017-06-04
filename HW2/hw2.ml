type ('terminal, 'nonterminal) symbol = 
	| T of 'terminal 
	| N of 'nonterminal;;
	
let rec to_gram2 rules sym =
	match rules with
	| [] -> []
	| (lhs, rhs)::t -> if lhs = sym then rhs::(to_gram2 t sym)
	                                else to_gram2 t sym;;

(* to_gram2 returns a function that, when given a symbol, returns
   the corresponding rules list *)
let convert_grammar gram1 =
	match gram1 with
	| (start, rules) -> (start, to_gram2 rules);;

let rec match_rules gram start rules acceptor d frag =
	(* rules that start symbol can reach *)
	match rules with 
	| [] -> None (* Rules exhausted. Failed to find a match. *)
	| h::t -> 
		match (match_rhs gram h acceptor (d@[start, h]) frag) with
		(* Failed to match the rule. Try the next one. *)
		| None -> (match_rules gram start t acceptor d frag )
		(* Success. Return whatever acceptor returns *)
		| x -> x

and match_rhs gram rule acceptor d frag =
	(* Check every symbol in the rule *)
	match rule with
	(* If it is a terminal, compare with frag *)
	| (T tm)::tr -> 
		(match frag with
		| [] -> None (* No more frag to match with rhs*)
		(* If current terminal matches, check next symbol.
		   Otherwise return None immediately. *)
		| hf::tf -> if hf = tm then match_rhs gram tr acceptor d tf
							   else None)
	(* If it is a nonterminal, call match_rules with the nonterminal
	   as the start symbol *)
	| (N ntm)::tr -> match_rules gram ntm (gram ntm) (match_rhs gram tr acceptor) d frag
	(* If we reach here, all the rules are matched successfully. Check with the acceptor. *)
	| [] -> acceptor d frag

let parse_prefix (start, gram) acceptor frag =
	match_rules gram start (gram start) acceptor [] frag;;
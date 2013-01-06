open Types

let list_of_string str =
	let rec iter acc = function
		| "" -> List.rev acc
		| s -> s.[0] :: acc
	in
	iter [] str

let getNode s =
	try
		Int (int_of_string s)
	with
		| Failure "int_of_string" -> Atom s

let tokenize str =
	let acc = ref "" in
	let inString = ref false in
	let inAtom = ref false in
	(*let inEscape = ref false in
	let inComment = ref false in*)

	let append s n =
	  (* append to a string ref *)
	  s := !s ^ (String.make 1 n)
	in
	let rest s =
	  String.sub s 1 ((String.length s) - 1)
	in

	let rec iter s xs =
	  match s with
	  	| "" ->
	  		(* end of string *)
	  		if String.length !acc > 0 then
	  		  List.rev |< (getNode !acc) :: xs
	  		else List.rev xs
	  	| _ ->
	  		(match s.[0] with
	  			| '"' when !inString ->
	  				(* string marker end *)
	  				let n = !acc in
	  				acc := "";
	  				inString := false;
	  				iter (rest s) |< (Str n) :: xs
	  			| '"' when not !inString && not !inAtom ->
	  				(* string marker begin *)
	  				inString := true;
	  				iter (rest s) xs
	  			| ' ' when not !inString ->
	  				if String.length !acc > 0 then
	  					(let atom = !acc in
	  					acc := "";
	  					inAtom := false;
	  					iter (rest s) ((getNode atom) :: xs))
	  				else iter (rest s) xs (* ignore multiple whitespace *)
	  			| c ->
	  				append acc c;
	  				if not !inString then
	  					inAtom := true
	  				else ();
	  				iter (rest s) xs
	  		)
	in
	Tokstream.create |< Array.of_list (iter str [])
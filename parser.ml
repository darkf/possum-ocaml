open Printf
open Types

let rec parseOne (ts : expr Tokstream.tokstream) (lookup : string -> expr option) : expr =
	let tok = Tokstream.consume ts in
	match tok with
		| None -> failwith "no more tokens"
		| Some tok ->
			match tok with
				| Atom s ->
					(* lookup atom to see if it's bound *)
					(match lookup s with
						| Some (Fun (a,_,_)) ->
							debug |< sprintf "parser: it's a fn (%s)" s;
							let args = parseSome ts lookup a in
							Call (s, args)
						| Some (SpecialForm(parsefn,_)) ->
							debug |< sprintf "parser: special form (%s)" s;
							parsefn ts lookup
						| _ -> tok (* just an atom - maybe a variable *)
					)
				| Str _ | Int _ | Nil -> tok
				| _ -> failwith ("invalid token: " ^ (sprintf_node 0 tok))

and parseSome ts lookup a =
	let rec iter acc = function
		| 0 -> List.rev acc
		| n -> iter ((parseOne ts lookup) :: acc) (n-1)
	in
	iter [] a


let parse ts lookup =
	let rec iter acc =
		match Tokstream.peek ts with
		| None -> List.rev acc
		| Some _ -> iter ((parseOne ts lookup) :: acc)
	in
	iter []
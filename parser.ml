open Printf
open Types

let rec parseOne (ts : expr Tokstream.tokstream) env : expr =
	let tok = Tokstream.consume ts in
	match tok with
		| None -> failwith "no more tokens"
		| Some tok ->
			match tok with
				| Atom s ->
					(* lookup atom to see if it's bound *)
					(match lookup env s with
						| Some (Fun (a,_,_)) ->
							debug |< sprintf "parser: it's a fn (%s)" s;
							let args = parseSome ts env a in
							Call (s, args)
						| Some (SpecialForm(parsefn,_)) ->
							debug |< sprintf "parser: special form (%s)" s;
							parsefn ts env
						| _ -> tok (* just an atom - maybe a variable *)
					)
				| Str _ | Int _ | Nil -> tok
				| _ -> failwith ("invalid token: " ^ (sprintf_node 0 tok))

and parseSome ts (env : env) a =
	let rec iter acc = function
		| 0 -> List.rev acc
		| n -> iter ((parseOne ts env) :: acc) (n-1)
	in
	iter [] a

and parseUntil ts env expr =
	let rec iter acc =
		match Tokstream.peek ts with
			| Some e when e = expr ->
				ignore |< Tokstream.consume ts; (* consume/ignore end token *)
				List.rev acc
			| Some _ ->
				let e = parseOne ts env in
				iter (e :: acc)
			| None -> failwith |< sprintf "Expected %s but EOS hit" (sprintf_node 0 expr)
	in
	iter []

and grabUntil ts expr =
	let rec iter acc =
		match Tokstream.peek ts with
			| Some e when e = expr ->
				ignore |< Tokstream.consume ts; (* consume/ignore end token *)
				List.rev acc
			| Some e ->
				ignore |< Tokstream.consume ts;
				iter (e :: acc)
			| None -> failwith |< sprintf "Expected %s but EOS hit" (sprintf_node 0 expr)
	in
	iter []


let parse ts (env : env) =
	let rec iter acc =
		match Tokstream.peek ts with
		| None -> List.rev acc
		| Some _ -> iter ((parseOne ts env) :: acc)
	in
	iter []
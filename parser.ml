open Printf
open Util
open Types

let rec parseOne (ts : expr Tokstream.tokstream) env : expr list =
	let tok = Tokstream.consume ts in
	match tok with
		| None -> failwith "no more tokens"
		| Some tok ->
			match tok with
				| Atom s ->
					(* lookup atom to see if it's bound *)
					(match lookup env s with
						| Some (Fun (a,_,_)) ->
							debug |< sprintf "parser: it's a fn: %s(%d)" s a;
							let args = parseSome ts env a in
							tok :: args
						| Some (SpecialForm(parsefn,_)) ->
							debug |< sprintf "parser: special form (%s)" s;
							tok :: (parsefn ts env)
						| _ -> [tok] (* just an atom - maybe a variable *)
					)
				| Str _ | Int _ | Nil -> [tok]
				| _ -> failwith ("invalid token: " ^ (repr_of_expr tok))

and parseSome ts (env : env) a =
	let rec iter acc = function
		| 0 -> acc
		| n -> iter (acc @ (parseOne ts env)) (n-1)
	in
	iter [] a

and parseUntil ts env expr =
	let rec iter acc =
		match Tokstream.peek ts with
			| Some e when e = expr ->
				ignore |< Tokstream.consume ts; (* consume/ignore end token *)
				acc
			| Some _ ->
				let e = parseOne ts env in
				iter (acc @ e)
			| None ->
				debug |< sprintf "ts: %s" (Tokstream.string_of_tokstream repr_of_expr ts);
				failwith |< sprintf "Expected %s but EOS hit" (repr_of_expr expr)
	in
	iter []

and parseUntilWith ts env expr =
	(parseUntil ts env expr) @ [expr]

and grabUntil ts expr =
	let rec iter acc =
		match Tokstream.peek ts with
			| Some e when e = expr ->
				ignore |< Tokstream.consume ts; (* consume/ignore end token *)
				List.rev acc
			| Some e ->
				ignore |< Tokstream.consume ts;
				iter (e :: acc)
			| None -> failwith |< sprintf "Expected %s but EOS hit" (repr_of_expr expr)
	in
	iter []

let parse ts (env : env) =
	let rec iter acc =
		match Tokstream.peek ts with
		| None -> List.rev acc
		| Some _ -> iter ((parseOne ts env) :: acc)
	in
	iter []
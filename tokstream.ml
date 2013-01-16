type 'a tokstream = {tokens : 'a array; len : int; mutable pos : int}

let create toks =
	{tokens=toks; len=Array.length toks; pos=0}

let peek t =
	match t.pos with
		| n when n >= t.len -> None
		| _ -> Some t.tokens.(t.pos)

let consume t =
	let tok = peek t in
	t.pos <- t.pos + 1;
	tok

let consumeUnsafe t =
	let tok = t.tokens.(t.pos) in
	t.pos <- t.pos + 1;
	tok
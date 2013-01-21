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

let tokstream_of_list lst =
	create (Array.of_list lst)

let string_of_tokstream f t =
	let v : 'a list = Array.to_list (Array.sub t.tokens t.pos (t.len-t.pos)) in
	Printf.sprintf "[%d of %d | %s]" t.pos t.len (String.concat "," (List.map f v))
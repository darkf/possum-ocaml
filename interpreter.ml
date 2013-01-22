open Printf
open Types

(* global symbol table *)
let genv : env = newEnv None

let _puts = function
	| [Str x] -> printf ": %s\n" x; Nil
	| _ -> failwith "puts: not an str"

let _print = function
	| [Str x] -> printf ": %s\n" x; Nil (* If it's just a string, don't repr it *)
	| [x] -> printf ": %s\n" (repr_of_expr x); Nil
	| _ -> failwith "print: wrong args"

let _repr = function
	| [x] -> Str (repr_of_expr x)
	| _ -> failwith "repr: wrong args"

let int_binop op = 
	Fun (2, [Atom "lhs"; Atom "rhs"],
		function
			| [Int lhs; Int rhs] -> Int (op lhs rhs)
			| _ -> failwith "not integers")

let _int_to_str = function
	| [Int i] -> Str (string_of_int i)
	| _ -> failwith "not an integer"

let _bool_to_str = function
	| [Bool b] -> Str (if b then "true" else "false")
	| _ -> failwith "not a boolean"

let _eq = function
	| [lhs; rhs] -> Bool (expr_equals lhs rhs)
	| _ -> failwith "need LHS/RHS"

let _concat = function
	| [Str lhs; Str rhs] -> Str (lhs ^ rhs)
	| _ -> failwith "need str LHS/RHS"

let rec evalNode tc env tok =
	match tok with
	| Atom s ->
		(match lookup env s with
			| Some (Fun (a,fargs,fn)) ->
				(* call a function *)
				debug |< sprintf "call to %s(%d)" s a;
				let xargs = List.map (fun _ -> evalOne tc env) fargs in
				debug |< sprintf "calling %s(%d) with args [%s]" s a (String.concat "," (List.map (sprintf_node 0) xargs));
				fn xargs (* call it *)
			| Some (SpecialForm (_, evalfn)) ->
				debug |< sprintf "calling special form %s" s;
				evalfn tc env (* call special form's evalfn *)
			| Some a -> a (* variable binding *)
			| None -> failwith |< sprintf "Unknown binding '%s'" s)
	| SpecialForm (_,_) -> failwith "Shouldn't have a special form directly"
	(* values *)
	| Str _ | Int _ | Bool _ | Fun _ | Nil -> debug ("returning value " ^ (sprintf_node 0 tok)); tok

and evalOne tc env =
	match Tokstream.consume tc with
		| Some tok -> evalNode tc env tok
		| None -> failwith "EOS"

and eval tc env =
	let rec iter acc =
		match Tokstream.peek tc with
		| None -> acc
		| Some _ ->
			iter ((evalOne tc env) :: acc)
	in
	List.hd |< iter [Nil]

let _set ts env =
	match Tokstream.consume ts with
		| Some (Atom name) ->
			let value = evalOne ts env in
			setSymFar env name value;
			value
		| _ -> failwith "set expected an atom"

let _ifParse ts env =
	let cond = Parser.parseOne ts env in
	let then1 = Parser.parseOne ts env in
	match Tokstream.peek ts with
		| Some (Atom "else") ->
			ignore |< Tokstream.consume ts;
			let then2 = Parser.parseOne ts env in
			cond @ then1 @ [Atom "else"] @ then2
		| _ ->
			cond @ then1

let _ifEval ts env =
	let cond = bool_of_expr (evalOne ts env) in
	let then1 = Parser.parseOne ts env in

	match Tokstream.peek ts with
		| Some (Atom "else") ->
			ignore |< Tokstream.consume ts; (* consume else *)
			if cond then
				let r = eval (Tokstream.tokstream_of_list then1) env in
				ignore |< Parser.parseOne ts env; (* consume else branch *)
				r
			else
				evalOne ts env (* evaluate else branch *)
		| _ ->
			if cond then
				eval (Tokstream.tokstream_of_list then1) env
			else Nil

let _quoteVar ts env =
	let tok = Tokstream.consume ts in
	match tok with
		| Some (Atom s) ->
			(match lookup env s with
				| Some a -> a
				| _ -> Nil)
		| _ -> failwith "quote-var: not a var"

let _defun ts env =
	debug |< "defun";
	let name_e : expr = Tokstream.consumeUnsafe ts in
	debug |< sprintf "defun: name: %s" (sprintf_node 0 name_e);
	match name_e with
		| Atom name ->
			let args : expr list = Parser.grabUntil ts (Atom "is") in
			debug |< sprintf "defun: args: [%s]" (String.concat ", " (List.map (sprintf_node 0) args));
			printf "CLS:\n";
			print_env 0 env;
			(* If we want to be able to mutate the closing scope, we just change the cloned environment to newEnv (Some env) *)
			let cls = copyEnv env in (* the closure environment *)
			let body : expr list = Parser.parseUntil ts cls (Atom "end") in
			debug |< sprintf "defun: body: %s" (String.concat ", " (List.map (sprintf_node 0) body));
			let arity = List.length args in
			(* We're going to build a closure to interpret the function, and then bind that. *)
			let fn (xargs : expr list) : expr =
				let cls_ = copyEnv cls in (* clone of the closure environment for this call *)
				for i = 0 to arity-1 do
					(match List.nth args i with
						| Atom arg -> setSymLocal cls_ arg |< List.nth xargs i
						| _ -> failwith "error: argument not an atom");
				done;
				let bodys = Tokstream.tokstream_of_list body in
				debug |< sprintf "evaluating %s(%d)'s body" name arity;
				eval bodys cls_
			in
			let f = Fun (arity, args, fn) in
			setSymLocal cls name f; (* bind to the closure for recursion purposes *)
			setSymLocal env name f;
			f
		| _ -> failwith "blah blah function name not an atom"

let setupStdlib () =
	setSymLocal genv "puts" |< Fun (1, [Atom "str"], _puts);
	setSymLocal genv "print" |< Fun (1, [Atom "value"], _print);
	setSymLocal genv "repr" |< Fun (1, [Atom "value"], _repr);
	setSymLocal genv "true" |< Bool true;
	setSymLocal genv "false" |< Bool false;
	setSymLocal genv "=" |< Fun (2, [Atom "lhs"; Atom "rhs"], _eq);
	setSymLocal genv "+" |< int_binop ( + );
	setSymLocal genv "-" |< int_binop ( - );
	setSymLocal genv "*" |< int_binop ( * );
	setSymLocal genv "/" |< int_binop ( / );
	setSymLocal genv "int->str" |< Fun (1, [Atom "x"], _int_to_str);
	setSymLocal genv "bool->str" |< Fun (1, [Atom "b"], _bool_to_str);
	setSymLocal genv "concat" |< Fun (2, [Atom "lhs"; Atom "rhs"], _concat);

	(* special forms *)
	setSymLocal genv "set!" |< SpecialForm ((fun ts env -> Parser.parseSome ts env 2), _set);
	setSymLocal genv "defun" |< SpecialForm ((fun ts env -> Parser.parseUntilWith ts env (Atom "end")), _defun);
	setSymLocal genv "quote-var" |< SpecialForm ((fun ts env -> Parser.parseOne ts env), _quoteVar);
	setSymLocal genv "if" |< SpecialForm (_ifParse, _ifEval)

let runTests () =
	let reset () =
	  (* empty the symtable and repopulate it *)
      Hashtbl.reset genv.sym;
      setupStdlib ()
    in
	let test_env () =
	  setSymLocal genv "x" (Int 10);
	  assert (Hashtbl.mem genv.sym "x");
	  (match lookup genv "x" with
	  | Some (Int 10) -> ()
	  | _ -> assert false);
	  setSymFar genv "f" (Int 90);
	  assert (Hashtbl.mem genv.sym "f");
	  let n = newEnv (Some genv) in
	  setSymLocal n "y" (Int 20);
	  (match lookup n "y" with
	   		| Some (Int 20) -> ()
	   		| _ -> assert false);
	  (match lookup n "x" with
			| Some (Int 10) -> ()
			| _ -> assert false);
	  setSymFar n "x" (Int 50);
	  (match lookup n "x" with
			| Some (Int 50) -> ()
			| _ -> assert false)
    in
    (*let test_arithmetic () =
      let thirty = Call ("+", [Int 10; Int 20]) in
      let p = Call ("int->str", [thirty]) in
      assert ((evalNode genv thirty) = (Int 30));
      assert ((evalNode genv p) = (Str "30"))
    in
    let test_parser () =
      let program = [Atom "print"; Atom "int->str"; Atom "+"; Int 10; Int 20] in
      let ts = Tokstream.create (Array.of_list program) in
      let p = Parser.parse ts genv in
      assert (p = [Call ("print", [
      						Call ("int->str", [
      							Call ("+", [Int 10; Int 20])
      						])])])
    in*)

    test_env ();
    reset ()
    (*test_arithmetic ();
    reset ();
    test_parser ()*)

let read_entire_file chan =
	let rec iter acc =
		try
			let line = (input_line chan) in
			iter (acc^line^"\n")
		with
			| End_of_file -> acc
	in
	iter ""

let () =
	setupStdlib ();
	runTests (); (* sanity tests *)
	printf "------------------------------------------------------------------\n";
	(*print_env 0 genv;*)
	let chan = open_in "examples/closures.psm" in
	let program = read_entire_file chan in
	let tc =  Tokenizer.tokenize program in (*"set! x 10 set! y 50 print int->str + x y" in*)
	printf "=== eval stage ===\n";
	printf "return: %s\n" |< sprintf_node 0 (eval tc genv)
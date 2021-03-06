open Printf
open Util
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

let _list_to_str = function
	| [Pair (_,_) as lst] -> Str (string_of_pairs lst)
	| _ -> failwith "list->str: not a pair"

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
				debug |< sprintf "calling %s(%d) with args [%s]" s a (String.concat "," (List.map repr_of_expr xargs));
				fn xargs (* call it *)
			| Some (SpecialForm (_, evalfn)) ->
				debug |< sprintf "calling special form %s" s;
				evalfn tc env (* call special form's evalfn *)
			| Some a -> a (* variable binding *)
			| None -> failwith |< sprintf "Unknown binding '%s'" s)
	| SpecialForm (_,_) -> failwith "Shouldn't have a special form directly"
	(* values *)
	| Str _ | Int _ | Bool _ | Fun _ | Struct _ | Pair _ | Nil -> debug ("returning value " ^ (repr_of_expr tok)); tok

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

let evalString program =
	let tc =  Tokenizer.tokenize program in
	eval tc genv

let _set ts env =
	match Tokstream.consume ts with
		| Some (Atom name) ->
			let value = evalOne ts env in
			setSymFar env name value;
			value
		| _ -> failwith "set expected an atom"

let _define ts env =
	match Tokstream.consume ts with
		| Some (Atom name) ->
			let value = evalOne ts env in
			setSymLocal env name value;
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

let _quoteVarParse ts env =
	match Tokstream.consume ts with
		| Some a -> [a]
		| None -> [Nil]

let _quoteVar ts env =
	let tok = Tokstream.consume ts in
	match tok with
		| Some (Atom s) ->
			(match lookup env s with
				| Some a -> a
				| _ -> Nil)
		| _ -> failwith "quote-var: not a var"

let _list ts env =
	let rec iter acc =
		match Tokstream.peek ts with
			| Some (Atom "end") ->
				ignore |< Tokstream.consume ts;
				List.rev acc
			| Some a ->
				iter ((evalOne ts env) :: acc)
			| None -> failwith "list expected end but got EOS"
	in
	let rec build = function
		| [] -> Nil
		| x::xs -> Pair (x, build xs)
	in
	build (iter [])

let _begin ts env =
	let rec iter acc =
		match Tokstream.peek ts with
			| Some (Atom "end") ->
				ignore |< Tokstream.consume ts;
				acc
			| Some a ->
				iter |< evalOne ts env
			| None -> failwith "begin: expected end, got EOS"
	in
	iter Nil

let _defstruct ts env =
	match Tokstream.consume ts with
		| Some (Atom name) ->
			(match Tokstream.consume ts with
				| Some (Atom "is") ->
					(* Struct definitions are structs themselves... struct-new just copies it,
					   vaguely like `clone` in prototypal languages *)
					let unatomize = (function | Atom s -> s | _ -> failwith "?") in
					let fields = List.map unatomize |< Parser.grabUntil ts (Atom "end") in
					let ht : (string, expr) Hashtbl.t = Hashtbl.create (List.length fields) in
					List.iter (fun s -> Hashtbl.add ht s Nil) fields; (* default to nil fields *)
					let v = Struct (fields, ht) in
					debug |< sprintf "binding struct: %s\n" name;
					setSymLocal env name v;
					v
				| _ -> failwith "defstruct: expected is"
			)
		| _ -> failwith "defstruct"

let _structnew ts env =
	match Tokstream.consume ts with
		| Some (Atom name) ->
			(match lookup env name with
				| Some (Struct (fields,_)) ->
					let ht = Hashtbl.create (List.length fields) in
					List.iter (fun k -> Hashtbl.add ht k (evalOne ts env)) fields;
					Struct (fields, ht)
				| _ -> failwith "struct-new: need a struct")
		| _ -> failwith "struct-new"

let _defun ts env =
	debug |< "defun";
	let name_e : expr = Tokstream.consumeUnsafe ts in
	debug |< sprintf "defun: name: %s" (repr_of_expr name_e);
	match name_e with
		| Atom name ->
			let args : expr list = Parser.grabUntil ts (Atom "is") in
			debug |< sprintf "defun: args: [%s]" (String.concat ", " (List.map repr_of_expr args));
			if !debugMode then
				(printf "CLS:\n";
				print_env 0 env);
			(* If we want to be able to mutate the closing scope, we just change the cloned environment to newEnv (Some env) *)
			let cls = copyEnv env in (* the closure environment *)
			let body : expr list = Parser.parseUntil ts cls (Atom "end") in
			debug |< sprintf "defun: body: %s" (String.concat ", " (List.map repr_of_expr body));
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
	setSymLocal genv "nil" |< Nil;
	setSymLocal genv "=" |< Fun (2, [Atom "lhs"; Atom "rhs"], _eq);
	setSymLocal genv "+" |< int_binop ( + );
	setSymLocal genv "-" |< int_binop ( - );
	setSymLocal genv "*" |< int_binop ( * );
	setSymLocal genv "/" |< int_binop ( / );
	setSymLocal genv "int->str" |< Fun (1, [Atom "x"], _int_to_str);
	setSymLocal genv "bool->str" |< Fun (1, [Atom "b"], _bool_to_str);
	setSymLocal genv "list->str" |< Fun (1, [Atom "list"], _list_to_str);
	setSymLocal genv "concat" |< Fun (2, [Atom "lhs"; Atom "rhs"], _concat);
	setSymLocal genv "nil?" |< Fun (1, [Atom "value"], function
														| [Nil] -> Bool true
														| _ -> Bool false);
	setSymLocal genv "cons" |< Fun (2, [Atom "lhs"; Atom "rhs"], function
																	| [x; y] -> Pair (x,y)
																	| _ -> failwith "cons: need LHS/RHS");
	setSymLocal genv "car" |< Fun (1, [Atom "pair"], function
														| [Pair (x,_)] -> x
														| _ -> failwith "car: need pair");
	setSymLocal genv "cdr" |< Fun (1, [Atom "pair"], function
														| [Pair (_,y)] -> y
														| _ -> failwith "cdr: need pair");
	setSymLocal genv "empty?" |< Fun (1, [Atom "value"], function
														| [Str s] -> Bool ((String.length s) = 0)
														| [Pair (x,_)] -> Bool (x = Nil)
														| [Nil] -> Bool true
														| _ -> failwith "empty?: need string or pair");
	setSymLocal genv "list-reverse" |< Fun(1, [Atom "list"], function
														| [Pair (_,_) as lst] -> list_reverse lst
														| _ -> failwith "list-reverse: need list");
	setSymLocal genv "chr" |< Fun (1, [Atom "n"], function
														| [Int n] -> Str (String.make 1 |< Char.chr n)
														| _ -> failwith "chr: needs int");
	setSymLocal genv "ord" |< Fun (1, [Atom "s"], function
														| [Str s] -> Int (Char.code s.[0])
														| _ -> failwith "ord: needs str");
	setSymLocal genv "struct-get" |< Fun (2, [Atom "struct"; Atom "field"], function
														| [Struct (_,values); Str field] -> Hashtbl.find values field
														| _ -> failwith "struct-get: need struct and string");

	setSymLocal genv "struct-set!" |< Fun (3, [Atom "struct"; Atom "field"; Atom "value"], function
														| [Struct (_,values); Str field; value] -> Hashtbl.replace values field value; value
														| _ -> failwith "struct-set!: need struct, string and value");


	(* special forms *)
	setSymLocal genv "set!" |< SpecialForm ((fun ts env -> Parser.parseSome ts env 2), _set);
	setSymLocal genv "define" |< SpecialForm ((fun ts env -> Parser.parseSome ts env 2), _define);
	setSymLocal genv "defun" |< SpecialForm ((fun ts env -> Parser.parseUntilWith ts env (Atom "end")), _defun);
	setSymLocal genv "quote-var" |< SpecialForm (_quoteVarParse, _quoteVar);
	setSymLocal genv "if" |< SpecialForm (_ifParse, _ifEval);
	setSymLocal genv "list" |< SpecialForm ((fun ts env -> Parser.parseUntilWith ts env (Atom "list")), _list);
	setSymLocal genv "begin" |< SpecialForm ((fun ts env -> Parser.parseUntilWith ts env (Atom "end")), _begin);
	setSymLocal genv "defstruct" |< SpecialForm ((fun ts env -> Parser.parseUntilWith ts env (Atom "end")), _defstruct);
	setSymLocal genv "struct-new" |< SpecialForm ((fun ts env -> Parser.parseUntil ts env (Atom "end")), _structnew);

	Io.register (setSymLocal genv)

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
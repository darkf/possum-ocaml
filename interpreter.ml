open Printf
open Types

(* global symbol table *)
let genv : env = newEnv None

let _printfn = function
	| [Str x] -> printf ": %s\n" x; Nil
	| _ -> failwith "print error"

let _plus = function
	| [Int lhs; Int rhs] -> Int (lhs+rhs)
	| _ -> failwith "not integers"

let _int_to_str = function
	| [Int i] -> Str (string_of_int i)
	| _ -> failwith "not an integer"

let rec evalNode env node = match node with
	| Call (name, args) ->
		(* call a function *)
		debug |< sprintf "call to %s with %s" name (sprintf_node 0  |< List.hd args);
		(match lookup env name with
			| Some (Fun (a, fargs, fn)) ->
				let xargs = List.map (evalNode env) args in
				debug |< sprintf "calling %s(%d) with args [%s]" name a (String.concat "," (List.map (sprintf_node 0) xargs));
				fn xargs (* call it *)
			| Some (SpecialForm (_, evalfn)) ->
				debug |< sprintf "calling special form %s with args [%s]" name (String.concat "," (List.map (sprintf_node 0) args));
				evalfn env args (* call special form's evalfn *)
			| Some _ -> failwith "non-fn called"
			| None -> failwith ("no such fn " ^ name)
		)
	| FunDef (a, args, body) -> printf "!!! todo: def"; Nil
	| SpecialForm (_,_) -> failwith "Shouldn't have a special form directly"
	| Atom s ->
		(match lookup env s with
			| Some a -> a
			| None -> failwith |< sprintf "Unknown binding '%s'" s)
	(* values *)
	| Str _ | Int _ | Fun _ | Nil -> debug ("returning value " ^ (sprintf_node 0 node)); node

let rec eval env ast =
	let rec iter acc = function
		| [] -> acc
		| x::xs ->
			iter ((evalNode env x) :: acc) xs
	in
	List.hd |< iter [] ast

let setupStdlib () =
	setSymLocal genv "print" |< Fun (1, [Atom "str"], _printfn);
	setSymLocal genv "+" |< Fun (2, [Atom "lhs"; Atom "rhs"], _plus);
	setSymLocal genv "int->str" |< Fun (1, [Atom "x"], _int_to_str);

	(* special forms *)
	setSymLocal genv "set!" |< SpecialForm ((fun ts lookup -> Call("set!", Parser.parseSome ts lookup 2)), (fun env args ->
																						match args with
																							| [Atom name; value] ->
																								setSymFar env name value;
																								value
																					   ))

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
    let test_arithmetic () =
      let thirty = Call ("+", [Int 10; Int 20]) in
      let p = Call ("int->str", [thirty]) in
      assert ((evalNode genv thirty) = (Int 30));
      assert ((evalNode genv p) = (Str "30"))
    in
    let test_parser () =
      let program = [Atom "print"; Atom "int->str"; Atom "+"; Int 10; Int 20] in
      let ts = Tokstream.create (Array.of_list program) in
      let p = Parser.parse ts (lookup genv) in
      assert (p = [Call ("print", [
      						Call ("int->str", [
      							Call ("+", [Int 10; Int 20])
      						])])])
    in

    test_env ();
    reset ();
    test_arithmetic ();
    test_parser ();
    reset ()

let () =
	setupStdlib ();
	runTests (); (* sanity tests *)
	printf "------------------------------------------------------------------\n";
	print_env 0 genv;

	(*let program = [Call ("print", [
									Call ("int->str", [
										Call ("+", [Int 10; Int 20])
									])
								  ])] in*)
	let ts =  Tokenizer.tokenize "set! x 10 set! y 50 print int->str + x y" (*"print int->str + 10 20"*) in
	let p = Parser.parse ts (lookup genv) in
	printf "AST:\n";
	print_ast p;
	printf "\n";
	printf "return: %s\n" |< sprintf_node 0 (eval genv p)
open Printf
open Types

(* global symbol table *)
let genv : env = newEnv None

let (|<) f v = f v

let runTests () =
	let test_env () =
	  assert ((Hashtbl.length genv.sym) = 0);
	  setSymLocal genv "x" (Int 10);
	  assert ((Hashtbl.length genv.sym) = 1);
	  assert (Hashtbl.mem genv.sym "x");
	  (match lookup genv "x" with
	  | Some (Int 10) -> ()
	  | _ -> assert false);
	  let n = newEnv (Some genv) in
	  setSymLocal n "y" (Int 20);
	  assert ((Hashtbl.length n.sym) = 1);
	  (match lookup n "y" with
	   		| Some (Int 20) -> ()
	   		| _ -> assert false);
	  (match lookup n "x" with
			| Some (Int 10) -> ()
			| _ -> assert false);
	  assert (setSymFar n "x" (Int 50));
	  (match lookup n "x" with
			| Some (Int 50) -> ()
			| _ -> assert false)
    in
    test_env ()

let _printfn = function
	| [Str x] -> printf ": %s\n" x; Nil
	| _ -> failwith "print error"

let rec evalNode env node = match node with
	| Call (name, args) ->
		(* call a function *)
		printf "! call %s with %s\n" name (sprintf_node 0  |< List.hd args);
		(match lookup env name with
			| Some (Fun (fargs, fn)) ->
				printf "! calling %s\n" name;
				fn (List.map (evalNode env) args)
			| Some _ -> failwith "non-fn called"
			| None -> failwith ("no such fn " ^ name)
		)
	| FunDef (args, body) -> printf "todo: def\n"; Nil
	(* values *)
	| Str _ | Int _ | Fun _ | Atom _ | Nil -> printf ">>>value\n"; node

let rec eval env = function
	| [] -> Nil
	| x::xs ->
		evalNode  env x

let () =
	runTests (); (* sanity tests *)

	setSymLocal genv "print" |< Fun ([Atom "str"], _printfn);
	print_env 0 genv;

	let program = [Call ("print", [Str "hi"])] in
	print_node 0 (eval genv program)
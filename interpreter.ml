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
			| Some _ -> failwith "non-fn called"
			| None -> failwith ("no such fn " ^ name)
		)
	| FunDef (a, args, body) -> printf "!!! todo: def"; Nil
	(* values *)
	| Str _ | Int _ | Fun _ | Atom _ | Nil -> debug ("returning value " ^ (sprintf_node 0 node)); node

let rec eval env = function
	| [] -> Nil
	| x::xs ->
		evalNode  env x

let setupStdlib () =
	setSymLocal genv "print" |< Fun (1, [Atom "str"], _printfn);
	setSymLocal genv "+" |< Fun (2, [Atom "lhs"; Atom "rhs"], _plus);
	setSymLocal genv "int->str" |< Fun (1, [Atom "x"], _int_to_str)

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
	  let n = newEnv (Some genv) in
	  setSymLocal n "y" (Int 20);
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
    let test_arithmetic () =
      let thirty = Call ("+", [Int 10; Int 20]) in
      let p = Call ("int->str", [thirty]) in
      assert ((evalNode genv thirty) = (Int 30));
      assert ((evalNode genv p) = (Str "30"))
    in

    test_env ();
    reset ();
    test_arithmetic ();
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
	let program = [Atom "print"; Atom "int->str"; Atom "+"; Int 10; Int 20] in
	let ts = Tokstream.create (Array.of_list program) in
	let p = Parser.parse ts (lookup genv) in
	print_ast p
	(*print_node 0 (eval genv program)*)
open Printf
open Types

(* global symbol table *)
let genv : env = newEnv None

let (|<) f v = f v

let program = [Call ("print", [Atom "hi"])]

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

let () =
	runTests ();

	setSymLocal genv "x" (Int 10);
	let n = newEnv (Some genv) in
	setSymLocal n "y" (Int 20);
	setSymFar n "x" (Int 30);
	print_env 0 n
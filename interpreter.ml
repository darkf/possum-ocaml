open Printf
open Types

(* global symbol table *)
let genv : env = newEnv None

let program = [Call ("print", [Atom "hi"])]

let () =
	setSymLocal genv "x" (Int 10);
	let n = newEnv (Some genv) in
	setSymLocal n "y" (Int 20);
	setSymFar n "x" (Int 30);
	print_env 0 n
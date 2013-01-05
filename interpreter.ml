open Printf
open Types

(* global symbol table *)
let genv : env = newEnv None

let program = [Call ("print", [Atom "hi"])]

let () =
	setSymLocal genv "x" (Int 10);
	print_env genv
open Printf
open Util
open Interpreter

let read_entire_file chan =
	let rec iter acc =
		try
			let line = (input_line chan) in
			iter (acc^line^"\n")
		with
			| End_of_file -> acc
	in
	iter ""

let evalFile file =
	let chan = open_in file in
	let program = read_entire_file chan in
	evalString program

let () =
	let files = ref [] in

	(* parse command-line arguments *)
	Array.iter (function
					| "--debug" ->
						debugMode := true
					| file ->
						if Sys.file_exists file then
							files := !files @ [file]
						else
							failwith |< "No such file: " ^ file
	) (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1));

	setupStdlib ();
	runTests (); (* sanity tests *)

	if List.length !files = 0 then
	begin
		(* REPL mode *)
		let rec iter () =
			printf "> ";
			match read_line () with
				| ":q" -> printf "bye\n"
				| line ->
					let result = evalString line in
					printf "%s\n" (Types.repr_of_expr result);
					iter ()
		in
		iter ()
	end
	else
		(* evaluate input files *)
		ignore |< List.map evalFile !files
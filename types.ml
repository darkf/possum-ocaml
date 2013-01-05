open Printf

(* This type is used as both an AST and for values.
   FunDef is used in the AST for defining functions with a body, and
   Fun is used for the function value type, which carries an internal function.

   The difference between regular functions and macros is that functions take in values as arguments,
   while macros take in a token stream as an argument and returns a new AST in place. *)

type expr = Call of string * expr list (* name, args *)
          | FunDef of expr list * expr list (* args, body, *)
          | Fun of expr list * (expr list -> expr)  (* args, fn - function value type *)
          | Atom of string
          | Str of string
          | Int of int
          | Nil

type envtbl = (string, expr) Hashtbl.t
type env = {sym : envtbl; prev : env option}

let newEnv prev = {sym=Hashtbl.create 32; prev=prev}

let lookupLocal env sym =
	try Some (Hashtbl.find env.sym sym)
	with
		Not_found -> None

let lookup first_env sym =
	let rec iter env =
	  match lookupLocal env sym with
	  	| (Some _) as v -> v
	  	| None ->
	  		match env.prev with
	  		| Some env -> iter env
	  		| None -> None
	in
	iter first_env

let setSymLocal env sym value =
	Hashtbl.replace env.sym sym value

let setSymFar first_env sym value =
	let rec iter env =
		if Hashtbl.mem env.sym sym then
			(Hashtbl.replace env.sym sym value;
			true)
		else
			match env.prev with
				| Some env -> iter env
				| None -> false
	in
	iter first_env

let print_t t =
	printf "%s" (String.make t ' ')

let sprintf_t t = String.make t ' '

let rec print_node t = function
	| Call (name,args) ->
	  print_t t;
	  printf "Call to %s\n" name;
	  List.iter (print_node (t+2)) args
	| Atom s ->
	  print_t t;
	  printf "Atom %s" s
	| Str s ->
	  print_t t;
	  printf "Str \"%s\"" s
	| Int i ->
	  print_t t;
	  printf "Int %d" i
	| FunDef (args,body) ->
	  print_t t;
	  printf "FunDef";
	  print_t (t+2);
	  printf "Args:";
	  List.iter (print_node (t+4)) args;
	  print_t (t+2);
	  printf "Body:";
	  List.iter (print_node (t+4)) body
	 | Fun (args,_) ->
	  print_t t;
	  printf "Fun";
	  print_t (t+2);
	  printf "Args:";
	  List.iter (print_node (t+4)) args
	 | Nil ->
	  print_t t;
	  printf "Nil"

let rec sprintf_node t = function
	| Call (name,args) ->
	  sprintf_t t ^ sprintf "Call to %s with [%s]" name (String.concat ", " (List.map (sprintf_node 0) args))
	| Atom s -> sprintf_t t ^ sprintf "<Atom %s>" s
	| Str s -> sprintf_t t ^ sprintf "<Str \"%s\">" s
	| Int i -> sprintf_t t ^ sprintf "<Int %d>" i
	| FunDef (args,body) -> sprintf_t t ^ sprintf "<FunDef args=[%s] body=...>" (String.concat ", " (List.map (sprintf_node 0) args))
	| Fun (args,_) -> sprintf_t t ^ sprintf "<Fun args=[%s]>" (String.concat ", " (List.map (sprintf_node 0) args))
	| Nil -> sprintf_t t ^ "Nil"

let rec print_env t = function
	| {sym=s; prev=p} ->
		print_t t;
		printf "<Environment>\n";

		print_t (t+2);
		printf "Symbols:\n";
		Hashtbl.iter (fun k v -> print_t (t+4); printf "%s -> %s\n" k (sprintf_node 0 v)) s;

		(match p with
			| Some env ->
				print_t (t+2);
				printf "Prev:\n";
				print_env (t+4) env
			| None -> ())

let print_ast ast =
	List.iter (print_node 0) ast
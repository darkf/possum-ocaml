open Printf

type expr = Fun of int * expr list * (expr list -> expr)  (* arity, args, fn - function value type *)
          | SpecialForm of (expr Tokstream.tokstream -> env -> expr list) * (expr Tokstream.tokstream -> env -> expr) (* parsefn, evalfn *)
          | Bool of bool
          | Atom of string
          | Str of string
          | Int of int
          | Nil

and envtbl = (string, expr) Hashtbl.t
and env = {sym : envtbl; prev : env option}

let newEnv prev = {sym=Hashtbl.create 32; prev=prev}
let copyEnv env = {sym=Hashtbl.copy env.sym; prev=env.prev}

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
			Hashtbl.replace env.sym sym value
		else
			match env.prev with
				| Some env -> iter env
				| None ->
					(* the binding doesn't exist in any upper scope, let's create one in the local scope *)
					Hashtbl.add first_env.sym sym value
	in
	iter first_env

let (|<) f v = f v
let debug = printf "debug: %s\n"

let print_t t =
	printf "%s" (String.make t ' ')

let sprintf_t t = String.make t ' '

let rec print_node t = function
	| Bool b ->
	  print_t t;
	  printf "Bool %s" (if b then "true" else "false")
	| Atom s ->
	  print_t t;
	  printf "Atom %s\n" s
	| Str s ->
	  print_t t;
	  printf "Str \"%s\"" s
	| Int i ->
	  print_t t;
	  printf "Int %d\n" i
	 | Fun (a,args,_) ->
	  print_t t;
	  printf "Fun(%d)" a;
	  print_t (t+2);
	  printf "Args:";
	  List.iter (print_node (t+4)) args
	 | SpecialForm(_,_) -> print_t t; printf "SF\n"
	 | Nil ->
	  print_t t;
	  printf "Nil"

let rec sprintf_node t = function
	| Bool b -> sprintf_t t ^ sprintf "<Bool %s>" (if b then "true" else "false")
	| Atom s -> sprintf_t t ^ sprintf "<Atom %s>" s
	| Str s -> sprintf_t t ^ sprintf "<Str \"%s\">" s
	| Int i -> sprintf_t t ^ sprintf "<Int %d>" i
	| Fun (a,args,_) -> sprintf_t t ^ sprintf "<Fun arity=%d args=[%s]>" a (String.concat ", " (List.map (sprintf_node 0) args))
	| SpecialForm(_,_) -> sprintf_t t ^ "SF"
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


let bool_of_expr = function
	| Bool b -> b
	| n -> failwith |< sprintf "todo: bool semantics for: %s" (sprintf_node 0 n)

let expr_equals lhs rhs =
	lhs = rhs
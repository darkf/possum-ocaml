open Printf
open Util

type expr = Fun of int * expr list * (expr list -> expr)  (* arity, args, fn - function value type *)
          | SpecialForm of (expr Tokstream.tokstream -> env -> expr list) * (expr Tokstream.tokstream -> env -> expr) (* parsefn, evalfn *)
          | Struct of string list * (string, expr) Hashtbl.t (* field names (for ordering), name -> value map of fields *)
          | Pair of expr * expr
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

let print_t t =
	printf "%s" (String.make t ' ')

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
	 | Pair (a,b) -> print_t t; printf "Pair\n"; print_node (t+2) a; print_node (t+2) b
	 | Struct _ -> print_t t; printf "Struct";
	 | Nil ->
	  print_t t;
	  printf "Nil"

let rec repr_of_expr = function
	| Bool b -> (if b then "true" else "false")
	| Atom s -> sprintf "<atom %s>" s
	| Str s -> sprintf "\"%s\"" s
	| Int i -> string_of_int i
	| Fun (a,_,_) -> sprintf "<fun(%d)>" a
	| SpecialForm(_,_) -> "<special form>"
	| Pair (a,b) -> sprintf "<pair %s, %s>" (repr_of_expr a) (repr_of_expr b)
	| Struct _ -> sprintf "<struct>"
	| Nil -> "nil"

let rec print_env t = function
	| {sym=s; prev=p} ->
		print_t t;
		printf "<Environment>\n";

		print_t (t+2);
		printf "Symbols:\n";
		Hashtbl.iter (fun k v -> print_t (t+4); printf "%s -> %s\n" k (repr_of_expr v)) s;

		(match p with
			| Some env ->
				print_t (t+2);
				printf "Prev:\n";
				print_env (t+4) env
			| None -> ())

let print_ast ast =
	List.iter (print_node 0) ast

let string_of_pairs lst =
	let rec iter = function
		| Pair (x,y) ->
			(match y with
				| Nil -> repr_of_expr x
				| _ -> (repr_of_expr x)^","^(iter y))
		| Nil -> ""
		| _ -> failwith "string_of_pairs: not a pair"
	in
	sprintf "[%s]" (iter lst)

let bool_of_expr = function
	| Bool b -> b
	| n -> failwith |< sprintf "todo: bool semantics for: %s" (repr_of_expr n)

let expr_equals lhs rhs =
	lhs = rhs

let list_reverse lst =
	let rec iter acc = function
		| Nil -> acc
		| Pair(x,xs) -> iter (Pair (x, acc)) xs
		| x -> failwith |< sprintf "list reverse: not a pair (%s)" (repr_of_expr x)
	in
	iter Nil lst
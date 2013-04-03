open Types
open Util

let is_stream = function
	| Struct (_,ht) ->
		(try
			match Hashtbl.find ht "read-bytes" with
				| Fun _ -> true
				| _ -> false
		with
			Not_found -> false)
	| _ -> false

let mkstruct fieldvalues =
	let ht = Hashtbl.create (List.length fieldvalues) in
	let rec iterpair = function
		| (k, v)::xs ->
			Hashtbl.add ht k v;
			iterpair xs
		| [] -> ()
	in
	iterpair fieldvalues;
	Struct ((List.map fst fieldvalues), ht)

let _stream_read = function
	| [Struct (_,ht) as s; Int _ as n] when is_stream s ->
		let readbytes = Hashtbl.find ht "read-bytes" in
		(match readbytes with
			| Fun (_,_,fn) ->
				fn [s; n]
			| _ -> failwith "stream-read: needs read-bytes")
	| _ -> failwith "stream-read: needs stream"

let _stream_write = function
	| [Struct (_,ht) as s; Str _ as n] when is_stream s ->
		let writebytes = Hashtbl.find ht "write-bytes" in
		(match writebytes with
			| Fun (_,_,fn) ->
				fn [s; n]
			| _ -> failwith "stream-write: needs write-bytes")
	| _ -> failwith "stream-write: needs stream"

let _stream_close = function
	| [Struct (_,ht) as s] when is_stream s ->
		let close = Hashtbl.find ht "close" in
		(match close with
			| Fun (_,_,fn) ->
				fn [s]
			| _ -> failwith "stream-close: needs close")
	| _ -> failwith "stream-close: needs stream"

let _file_open = function
	| [Str filename] ->
		(try
			let fd = Unix.openfile filename [Unix.O_RDWR; Unix.O_CREAT] 0o777 in
			let readbytes = function
				| [Struct _ as s; Int n] when is_stream s ->
					let buf = String.create n in
					let rd = Unix.read fd buf 0 n in
					Str (String.sub buf 0 rd)
				| _ -> failwith "read-bytes: needs a stream and a length"
			in
			let writebytes = function
				| [Struct _ as s; Str data] when is_stream s ->
					let len = String.length data in
					let rd = Unix.write fd data 0 len in
					Int rd
				| _ -> failwith "write-bytes: needs a stream and data"
			in
			let close = function
				| [Struct _ as s] when is_stream s ->
					Unix.close fd;
					Nil
				| _ -> failwith "close: needs a stream"
			in
			mkstruct [("read-bytes", Fun (2, [Atom "s"; Atom "n"], readbytes));
					  ("write-bytes", Fun (2, [Atom "s"; Atom "d"], writebytes));
					  ("close", Fun (1, [Atom "s"], close))]
		with
			| Unix.Unix_error (e, _, _) ->
				failwith |< Unix.error_message e
			| e -> raise e)
	| _ -> failwith "file-open: need string filename"

let register bind =
	bind "stream-read" |< Fun (2, [Atom "stream"; Atom "n"], _stream_read);
	bind "stream-write" |< Fun (2, [Atom "stream"; Atom "data"], _stream_write);
	bind "stream-close" |< Fun (1, [Atom "stream"], _stream_close);
	bind "file-open" |< Fun (1, [Atom "filename"], _file_open)
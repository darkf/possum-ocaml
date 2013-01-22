let debugMode = ref false

let debug msg =
	if !debugMode then
		Printf.printf "debug: %s\n" msg
	else ()

let (|<) f v = f v
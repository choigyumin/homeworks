exception TODO

type pgm = cmd
and cmd = ASSIGN of exp
 | SEQUENCE of cmd * cmd
 | REPEAT of cmd
 | CHOICE of cmd * cmd
 | EQ of exp * cmd
 | NEQ of exp * cmd
and exp = NUM of int
 | ADD of exp * exp
 | SUB of exp * exp
 | VAR

type state = int



let rec exeval (p:pgm) (st:state): state list =
	let rec calc (e:exp): int=
		match e with
			| NUM a -> a
			| ADD (a,b) -> (calc a) + (calc b)
			| SUB (a,b) -> (calc a) - (calc b)
			| VAR -> st
		in
	let rec seqeval (p:state list) (q:pgm): state list=
		match p with
			| [] -> []
			| hd::tl -> List.sort_uniq compare (List.append (exeval q hd) (seqeval tl q))
		in
	match st with
	| st ->
		if (st >= -5) &&
		(st <= 5) then 
		(match p with
		| ASSIGN a -> [(calc a)]
		| SEQUENCE (a,b) ->
			seqeval (exeval a st) b
		| REPEAT a -> 
			if (exeval a st) = (seqeval (exeval a st) a) then (seqeval (exeval a st) a)
			else List.sort_uniq compare (List.append (exeval a st) (seqeval (exeval a st) a))
		| CHOICE (a,b) -> List.sort_uniq compare (List.append (exeval a st) (exeval b st))
		| EQ (a,b) -> 
			if (calc a) = st then (exeval b st)
			else []
		| NEQ (a,b) -> 
			if (calc a) <> st then (exeval b st)
			else [])
	  
		else []

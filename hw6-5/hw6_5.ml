exception TODO

 type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (e: ae) (x: string): ae =
  match e with
	| CONST a -> CONST 0
	| VAR a ->
		if a = x then CONST 1
		else CONST 0
	| POWER (a,b) -> 
		(match (a,b) with
		| (x,0) -> CONST 0
		| (x,_) -> TIMES [CONST b; POWER (a, b-1)]
		| (_,_) -> CONST 0)
	| TIMES lst ->
		(match lst with
		| [] -> CONST 0
		| hd::tl -> TIMES (hd::(List.map (fun(list)-> (diff list x)) tl)))

	| SUM lst ->
		(match lst with
		| [] -> CONST 0
		| hd::tl -> SUM ((diff hd x)::(List.map (fun(list)-> (diff list x)) tl)))


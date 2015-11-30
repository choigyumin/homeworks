exception TODO

type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec calc (e: expr): int =
	match e with
	| NUM a -> a
	| PLUS (a, b) -> (calc a+ calc b)
	| MINUS (a, b) -> (calc a- calc b)

let rec eval (f: formula): bool =
  match f with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> not (eval a)
	| ANDALSO (a, b) -> (eval a) && (eval b)
	| ORELSE (a, b) -> (eval a) || (eval b)
	| IMPLY (a, b) -> 
		if (eval a = true && eval b = false) then false
		else true
	| LESS (a, b) ->
		match (a,b) with
		| (NUM n, NUM m) -> 
			if n < m then true 
			else false
		| _ ->
			if calc a < calc b then true
			else false
			


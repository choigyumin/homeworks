exception TODO

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna


let team_to_string (a:team): string =
	match a with
	| Korea -> "Korea"
	| France -> "France"
	| Usa -> "Usa"
	| Brazil -> "Brazil"
	| Japan -> "Japan"
	| Nigeria -> "Nigeria"
	| Cameroon -> "Cameroon"
  | Poland -> "Poland"
	| Portugal -> "Portugal"
	| Italy -> "Italy"
	| Germany -> "Germany"
	| Norway -> "Norway"
	| Sweden -> "Sweden"
	| England -> "England"
  | Argentina -> "Argentina"

let rec parenize (t: tourna): string =
  match t with
	| LEAF a -> team_to_string a
	| NODE (a,b) -> "(" ^ (parenize a) ^ " " ^ (parenize b) ^ ")"

let rec drop_h ((t: tourna), (d: team)): tourna =
	match (t, d) with
	| (LEAF a, d) -> LEAF a
	| (NODE (a,b), d) -> 
		(match (a,b,d) with
		| (LEAF z, LEAF x, d) ->
			if team_to_string z = team_to_string d then LEAF x
			else if team_to_string x = team_to_string d then LEAF z
			else NODE (LEAF z, LEAF x)
		| (LEAF z, NODE (c,v), d) ->
			if team_to_string z = team_to_string d then NODE (c,v)
			else NODE (LEAF z, drop_h (NODE(c,v), d))
		| (NODE (c,v), LEAF z, d) ->
			if team_to_string z = team_to_string d then NODE (c,v)
			else NODE (drop_h (NODE(c,v), d), LEAF z)
		| (NODE (c,v), NODE (b,n), d) ->
			NODE (drop_h (NODE(c,v), d), drop_h (NODE(b,n), d)))
		

let rec drop (t: tourna) (d: team): string =
  match(t, d) with
	| (LEAF a, d) ->
		if team_to_string a = team_to_string d then ""
		else team_to_string a
	| (NODE (a,b), d) -> parenize (drop_h (NODE (a,b), d))

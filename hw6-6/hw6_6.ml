exception TODO

type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
 and name = string

let rec stn_list (s: metro): string list =
	match s with
	| STATION a -> [a]
	| AREA(a,b) -> stn_list b
	| CONNECT (a,b) -> 
		List.append (stn_list a) (stn_list b)

let rec area_list (a: metro): string list =
	match a with
	| AREA (b,c) -> [b]
	| STATION b -> ["NOT a AREA"]
	| CONNECT (b,c) -> 
		List.append (area_list b) (area_list c)

let rec check_in (li: string list) (lo: string list): bool=
	match lo with
	| [] -> true
	| hd::tl -> (List.mem hd li) && (check_in li tl)

let rec checkMetro (m:metro): bool =
  match m with
	| STATION a -> true
	| _ ->
		check_in (area_list m) (stn_list m)
			
			
			
			
			
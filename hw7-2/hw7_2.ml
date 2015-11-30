module type SKI = sig
  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)
  val react: liquid -> liquid
  val pprint: liquid -> string
end

module SkiLiquid : SKI = struct

  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)

  let rec react: liquid -> liquid =
    fun l ->
			match l with
			| M(a,b) ->
				(match a with
				| I -> react b
				| M(c,d) -> 
					(match c with
					| K -> react d
					| M(e,f) ->
						if e = S then react (M(M(f,b), M(d,b)))
						else l
					| _ -> l)
				| _ -> l)
			| _ -> l
				

  let rec pprint: liquid -> string =
    fun l ->
			match l with
			| S -> "S"
			| K -> "K"
			| I -> "I"
			| V a -> a
			| M(a,b) -> "(" ^ pprint a ^ " " ^ pprint b ^ ")"
end

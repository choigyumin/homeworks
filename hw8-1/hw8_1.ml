module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end


module StringQ : Queue with type element = string = 
struct 
  type queue =
		| Q of part * part
	and part = element list
	and element = string
	exception EMPTY_Q
	
	let emptyq: queue = 
	  Q ([],[])
	
	let enq: queue * element -> queue =
		fun (q,e) ->
			match q with
			| Q (a,b) -> Q( e::a, b)
	
	
	let rec deq: queue -> element * queue =
		fun q ->
			match q with
			| Q(a,b) -> 
				(match (a,b) with
				| ([],[]) -> raise EMPTY_Q, Q([],[])
				| (_, []) -> deq (Q(b,List.rev a))
				| (_,_) -> List.hd b , Q(a, List.tl b))
		
end

module StringQQ : Queue with type element = StringQ.queue = 
struct 
  type queue =
	| Q of part * part
	and part = element list
	and element = StringQ.queue
	exception EMPTY_Q
	
	let emptyq: queue = 
	  Q ([],[])
	
	let enq: queue * element -> queue =
		fun (q,e) ->
			match q with
			| Q (a,b) -> Q( e::a, b)
	
	
	let rec deq: queue -> element * queue =
		fun q ->
			match q with
			| Q(a,b) -> 
				(match (a,b) with
				| ([],[]) -> raise EMPTY_Q, Q([],[])
				| (_, []) -> deq (Q(b,List.rev a))
				| (_,_) -> List.hd b , Q(a, List.tl b))
		
end
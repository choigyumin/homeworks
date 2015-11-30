module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringSetQ : Queue with type element = string = 
struct 
  type queue =
		| Q of part * part
	and part = element list
	and element = string
	exception EMPTY_Q
	
	let emptyq: queue = 
	  Q ([],[])
	
	let recog: queue -> queue =
		fun e ->
			match e with
			| Q(a,b) -> 
				(match (a,b) with
				| ([],_) -> e
				| _ -> Q([], b@(List.rev a)))


	let enq: queue * element -> queue =
		fun (q,e) ->
			match q with
			| Q (a,b) ->
				if ((List.mem e a) || (List.mem e b)) then recog (Q(a, b))
				else recog (Q(e::a, b))
	
	
	let rec deq: queue -> element * queue =
		fun q ->
			match q with
			| Q(a,b) -> 
				(match (a,b) with
				| ([],[]) -> raise EMPTY_Q, Q([],[])
				| (_, []) -> deq (Q(b,List.rev a))
				| (_,_) -> List.hd b , Q(a, List.tl b))
end

module StringSetQQ : Queue with type element = StringSetQ.queue = 
struct 
  type queue =
		| Q of part * part
	and part = element list
	and element = StringSetQ.queue
	exception EMPTY_Q
	
	let emptyq: queue = 
	  Q ([],[])
	
	let enq: queue * element -> queue =
		fun (q,e) ->
			match q with
			| Q (a,b) ->
				if ((List.mem e a) || (List.mem e b)) then Q(a, b)
				else Q(e::a, b)
	
	let rec deq: queue -> element * queue =
		fun q ->
			match q with
			| Q(a,b) -> 
				(match (a,b) with
				| ([],[]) -> raise EMPTY_Q, Q([],[])
				| (_, []) -> deq (Q(b,List.rev a))
				| (_,_) -> List.hd b , Q(a, List.tl b))
end

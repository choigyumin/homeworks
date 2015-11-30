module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyq : queue
    val enq : queue * element -> queue
    val deq : queue -> element * queue
  end
module StringSetQ :
  sig
    type element = string
    type queue
    exception EMPTY_Q
    val emptyq : queue
    val enq : queue * element -> queue
    val deq : queue -> element * queue
  end
module StringSetQQ :
  sig
    type element = StringSetQ.queue
    type queue
    exception EMPTY_Q
    val emptyq : queue
    val enq : queue * element -> queue
    val deq : queue -> element * queue
  end

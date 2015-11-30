module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyq : queue
    val enq : queue * element -> queue
    val deq : queue -> element * queue
  end
module StringQ :
  sig
    type element = string
    type queue
    exception EMPTY_Q
    val emptyq : queue
    val enq : queue * element -> queue
    val deq : queue -> element * queue
  end
module StringQQ :
  sig
    type element = StringQ.queue
    type queue
    exception EMPTY_Q
    val emptyq : queue
    val enq : queue * element -> queue
    val deq : queue -> element * queue
  end

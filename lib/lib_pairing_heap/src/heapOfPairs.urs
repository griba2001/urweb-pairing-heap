signature PRIO_Q_OF_PAIRS = sig
  con t :: Type
  con prio :: Type
  con payload :: Type

  val findTop: t -> option (prio * payload)
  val merge: t -> t -> t
  val insert: (prio * payload) -> t -> t
  val deleteTop: t -> t
end
(* heap of pairs (prio, payload) *)

signature HEAP_OF_PAIRS = sig
  include PRIO_Q_OF_PAIRS

  val empty: t
  val null: t -> bool
  val size: t -> int
  val member: eq prio -> eq payload -> prio * payload -> t -> bool

  val delete: eq prio -> eq payload -> prio * payload -> t -> t
  val decreaseKey: eq prio -> eq payload -> (prio -> prio) -> prio * payload -> t -> t

  val fromList: list (prio * payload) -> t
  val toList: t -> list (prio * payload)
  val show_heap: show prio -> show payload -> show t
end

functor MkMinHeapOfPairs(Q: sig
                con prio :: Type
                con payload :: Type
                val ord_prio : ord prio
             end):(HEAP_OF_PAIRS where con prio = Q.prio
                                 where con payload = Q.payload)

functor MkMaxHeapOfPairs(Q: sig
                con prio :: Type
                con payload :: Type
                val ord_prio : ord prio
             end):(HEAP_OF_PAIRS where con prio = Q.prio
                                 where con payload = Q.payload)
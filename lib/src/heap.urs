

signature PRIO_Q = sig
  con t :: Type
  con item :: Type

  val findTop: t -> option item
  val merge: t -> t -> t
  val insert: item -> t -> t
  val deleteTop: t -> t
end


signature HEAP = sig
  include PRIO_Q

  val empty: t
  val null: t -> bool
  val size: t -> int

  val fromList: list item -> t
  val toList: t -> list item
  val show_heap: show item -> show t

  (* invariants *)

  val propHeap: t -> bool
  val propFromToList: eq item -> list item -> bool
end

functor MkMinHeap(Q: sig
                con item :: Type
                val ord_item : ord item
             end):(HEAP where con item = Q.item)

functor MkMaxHeap(Q: sig
                con item :: Type
                val ord_item : ord item
             end):(HEAP where con item = Q.item)


(* heap of pairs (prio, payload) *)

signature PRIO_Q_OF_PAIRS = sig
  con t :: Type
  con prio :: Type
  con payload :: Type

  val findTop: t -> option (prio * payload)
  val merge: t -> t -> t
  val insert: (prio * payload) -> t -> t
  val deleteTop: t -> t
end


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

functor PairingHeapOfPairs(Q: sig
                con prio :: Type
                con payload :: Type
                val order: prio -> prio -> bool
             end):(HEAP_OF_PAIRS where con prio = Q.prio
                                 where con payload = Q.payload) = struct

open Q
structure L = List
structure O = Option
structure S = String
open HTuple

datatype entry = Entry of prio * payload


fun fromEntry (Entry pair) = pair

val getPrio: entry -> prio = fromEntry >>> fst

val entry_ord (_:ord prio): ord entry =
   let
     mkOrd {Lt = entry_lt, Le = entry_le}
   where
     fun entry_lt (e1: entry) (e2: entry) = getPrio e1 < getPrio e2
     fun entry_le (e1: entry) (e2: entry) = getPrio e1 <= getPrio e2
   end

val entry_eq (_:eq prio) (_:eq payload): eq entry =
   let
     mkEq eq'
   where
     fun eq' (e1: entry) (e2: entry) =

             fromEntry e1 = fromEntry e2
   end

val show_entry (_:show prio) (_:show payload): show entry =
   let mkShow show'
   where
     val show': entry -> string = fromEntry >>> show
   end


fun withPrioOrder (f: prio -> prio -> bool) (e1: entry) (e2: entry): bool =

      f (getPrio e1) (getPrio e2)

structure H = Heap.PairingHeap( struct
                    type item = entry
                    val order = withPrioOrder order
                  end)

type t = H.t

val findTop: t -> option (prio * payload) = H.findTop >>> O.mp fromEntry

val merge: t -> t -> t = H.merge

fun insert (pair: prio * payload): t -> t = H.insert (Entry pair)

val deleteTop: t -> t = H.deleteTop

val empty: t = H.empty
val null: t -> bool = H.null
val size: t -> int = H.size

fun member (_:eq prio) (_:eq payload) (pair: prio * payload): t -> bool = H.member (Entry pair)

fun delete (_:eq prio) (_:eq payload) (pair: prio * payload): t -> t = H.delete (Entry pair)


fun decreaseKey (_:eq prio) (_:eq payload) (f: prio -> prio) (pair: prio * payload): t -> t =
      let H.decreaseKey f_entry (Entry pair)
      where
        fun f_entry (Entry (prio, v)) = Entry (f prio, v)
      end


val fromList: list (prio * payload) -> t = L.mp Entry >>> H.fromList

val toList: t -> list (prio * payload) = H.toList >>> L.mp fromEntry

val show_heap (_:show prio) (_:show payload): show t =
    let mkShow show'
    where
      fun show' (h: t) = "fromList: " `S.append` show (toList h)
    end

end

(* --------------- *)

functor MkMinHeapOfPairs(Q: sig
                con prio :: Type
                con payload :: Type
                val ord_prio : ord prio
             end):(HEAP_OF_PAIRS where con prio = Q.prio
                                 where con payload = Q.payload) = struct

  open PairingHeapOfPairs(struct
                        type prio = Q.prio
                        type payload = Q.payload
                        val order = le
                   end)
end


functor MkMaxHeapOfPairs(Q: sig
                con prio :: Type
                con payload :: Type
                val ord_prio : ord prio
             end):(HEAP_OF_PAIRS where con prio = Q.prio
                                 where con payload = Q.payload) = struct

  open PairingHeapOfPairs(struct
                        type prio = Q.prio
                        type payload = Q.payload
                        val order = ge
                   end)
end

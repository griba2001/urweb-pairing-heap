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
  val member: eq item -> item -> t -> bool

  val delete: eq item -> item -> t -> t
  val decreaseKey: eq item -> (item -> item) -> item -> t -> t

  val fromList: list item -> t
  val toList: t -> list item
  val show_heap: show item -> show t

  (* invariants *)

  val propHeap: t -> bool
  val propFromToList: eq item -> list item -> bool
  val propAllMembers: eq item -> list item -> bool
  val propCheckAfterDeletes: eq item -> list item -> bool
  val propDecreasedKeysAreMembers: eq item -> (item -> item) -> list item -> bool
end


functor PairingHeap(Q: sig
                con item :: Type
                val order: item -> item -> bool
             end):(HEAP where con item = Q.item) = struct

structure O = Option
structure L = List
structure S = String

open HFunction
open HTuple

open Q

datatype node = Heap of item * list (option node)

type t = option node

val empty: t = None

val null: t -> bool = O.isNone

fun size (h1: t): int =
    case h1 of
      None => 0
      | Some (Heap (key, subs)) => 1 + L.foldl (size >>> plus) 0 subs

val heapTop: node -> item = fn (Heap (k, _)) => k

val findTop: t -> option item = O.mp heapTop

fun merge (h1: t) (h2: t): t =

     case (h1, h2) of
       | (None, None) => None
       | (Some _, None) => h1
       | (None, Some _) => h2
       | (Some (Heap (k1, subs1)), Some (Heap (k2, subs2))) =>
              if k1 `order` k2
                 then Some <| Heap (k1, h2 :: subs1)
                 else Some <| Heap (k2, h1 :: subs2)

fun insert (x: item) (h1: t): t = merge h1 <| Some <| Heap (x, [])

fun mergePairs (li: list t): t =
  case li of
    | [] => None
    | h1 :: [] => h1
    | h1 :: h2 :: rest => merge (merge h1 h2) <| mergePairs rest

fun deleteTop (h1: t): t =
   case h1 of
     None => None
     | Some (Heap (key, subs)) => mergePairs subs

fun member (_:eq item) (k1: item) (h1: t): bool =
   case h1 of
     None => False
     | Some (Heap (key, subs)) =>
         if key = k1 then True
         else if order key k1 then L.exists (member k1) subs
         else False

fun delete (_:eq item) (k1: item): t -> t =
    let delete' >>> fst
    where
        fun delete' (h: t): t * bool =
        case h of
          None => (None, False)
          | Some (Heap (key, subs)) =>
                if key = k1 then (mergePairs subs, True)
                else if order key k1 then
                        let
                          val (subs', deleted) = L.foldlMap deleteIfNotYet False subs
                          val newHeap = if deleted
                                        then Some (Heap (key, subs'))
                                        else h
                        in
                           (newHeap, deleted)
                        end
                else (h, False)

        and deleteIfNotYet (h: t) (done: bool): t * bool =
                if done
                then (h, done)
                else delete' h
    end

fun decreaseKey (_:eq item) (f: item -> item) (old: item): t -> t =
    delete old >>> insert (f old)

fun fromList (li: list item): t = L.foldl insert empty li

fun toList (h1: t): list item =
  let toList' [] h1
  where
   fun toList' (acc: list item) (h: t)  =
    case h of
      None => L.rev acc
      | Some (Heap (key, subs)) => toList' (key :: acc) (mergePairs subs)
  end 

(* class instances *)

val show_heap (_: show item): show t =
     let mkShow show'
     where
         fun show' (h: t): string = "fromList: " `S.append` show (toList h)
     end 

(* invariants *)

fun propHeap (h1: t): bool =
  let case h1 of
    None => True
    | Some (Heap (key, subs)) => L.all (subHeapIsOrdered key) subs
  where
    fun subHeapIsOrdered (parent: item) (sub: t): bool =
       case sub of
         None => True
         | Some (Heap (key, subs)) => parent `order` key && L.all (subHeapIsOrdered key) subs
  end

fun propFromToList (_: eq item) (li: list item): bool =

      toList (fromList li) = L.sort (fn x y => not <| order x y) li

fun propAllMembers (_: eq item) (li: list item): bool =
    let L.all (flip member heap) li
    where
      val heap = fromList li
    end  

fun propCheckAfterDeletes (_: eq item) (li: list item): bool =
      let
          val (itemsToDel, itemsNotToDel) = L.splitAt (L.length li / 2) li
          val heap = L.foldl delete (fromList li) itemsToDel
      in
         toList heap = L.sort (fn x y => not <| order x y) itemsNotToDel
      end

fun propDecreasedKeysAreMembers  (_: eq item) (f: item -> item) (li: list item): bool =
      let
          val oldItems = L.take (L.length li / 2) li
          val newItems = L.mp f oldItems
          val heap = L.foldl (decreaseKey f) (fromList li) oldItems
      in
          L.all (flip member heap) (L.mp f oldItems)  
      end

end

functor MkMinHeap(Q: sig
                con item :: Type
                val ord_item : ord item 
             end):(HEAP where con item = Q.item) = struct

  open PairingHeap(struct
                        type item = Q.item
                        val order = le
                   end)
end 

functor MkMaxHeap(Q: sig
                con item :: Type
                val ord_item : ord item
             end):(HEAP where con item = Q.item) = struct

  open PairingHeap(struct
                        type item = Q.item
                        val order = ge
                   end)
end

# urweb-pairing-heap

minHeap and maxHeap for items

with functions: findTop, merge, insert, deleteTop,

empty, null, size, fromList, toList

```ocaml
(* invariants *)

val propHeap: t -> bool                             (* node subHeaps order *)
val propFromToList: eq item -> list item -> bool    (* content conservation *)
```

### Usage

```ocaml
(* max heap: priority to the major *)
structure MaxH = Heap.MkMaxHeap( struct 
                               type item = int
                               val ord_item = ord_int
                           end)
val test1 = 1 :: 2 :: 3 :: []

val maxPriorityList = MaxH.toList <| MaxH.fromList test1

(* min heap: priority to the minor *)
structure MinH = Heap.MkMinHeap( struct
                               type item = int
                               val ord_item = ord_int
                           end)

val test2 = 3 :: 2 :: 1 :: []

val minPriorityList = MinH.toList <| MinH.fromList test2
```
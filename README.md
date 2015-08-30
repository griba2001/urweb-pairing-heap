# urweb-pairing-heap

minHeap and maxHeap for items and also for pairs (priority, payload)

with functions: findTop, merge, insert, deleteTop, delete, decreaseKey,

empty, null, size, fromList, toList

```ocaml
(* invariants *)

val propHeap: t -> bool                             (* node subHeaps order *)
val propFromToList: eq item -> list item -> bool    (* content conservation *)
val propCheckAfterDeletes: eq item -> list item -> bool  (* content conservation after deleting half of original list *)
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

structure MinHP = HeapOfPairs.MkMinHeapOfPairs (struct
                                                  type prio = int
                                                  type payload = string
                                                  val ord_prio = ord_int
                                                end)

val test3 = (3, "c") :: (2, "b") :: (1, "a") :: []

val minPriorityListOfPairs = (MinHP.toList <<< MinHP.fromList) test3

structure MaxHP = HeapOfPairs.MkMaxHeapOfPairs (struct
                                                  type prio = int
                                                  type payload = string
                                                  val ord_prio = ord_int
                                                end)

val test4 = (1, "c") :: (2, "b") :: (3, "a") :: []

val maxPriorityListOfPairs = (MaxHP.toList <<< MaxHP.fromList) test4
```

### test task

tests lib/test/heap_UnitTest UrUnit assertions

```bash
urweb test1
./test1.exe -p 8082
browser http://localhost:8082
```
Every browser page refresh brings different random input data

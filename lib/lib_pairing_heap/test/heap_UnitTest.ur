structure U = HUrUnit
structure L = List
structure H = Heap.MkMaxHeap( struct
                               type item = int
                               val ord_item = ord_int
                           end)

open HFunction

fun unitTest (testdata: list int): transaction (xbody * string * string) =
    let
        val myHeap = H.fromList testdata
        val fromTo = H.toList myHeap
        val myHeap2 = case testdata of
                        [] => myHeap
                        | hd :: rest => H.decreaseKey (flip minus 10) hd myHeap
    in
        tst0 <- U.assertBool "propHeap fails" (H.propHeap myHeap) ;
        tst1 <- U.assertBool "propFromToList fails" (H.propFromToList testdata) ;
        tst2 <- U.assertBool "propAllMembers fails" (H.propAllMembers testdata) ;
        tst3 <- U.assertBool "propCheckAfterDeletes fails" (H.propCheckAfterDeletes testdata) ;
        tst4 <- U.assertBool "propDecreasedKeysAreMembers fails" (H.propDecreasedKeysAreMembers (flip minus 10) testdata) ;
        let
            val testsResults = tst0 :: tst1 :: tst2 :: tst3 :: tst4 :: []
            val xmlJoinedResults = List.foldr join <xml/> testsResults
        in
            return (xmlJoinedResults, show myHeap, show myHeap2)
        end
     end

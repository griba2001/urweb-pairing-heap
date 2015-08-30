
structure HSR = HSRandom

structure T = Heap_UnitTest

fun main () =
        testdata <- HSR.getSysRandomIntList 30 0 100 ;
        let val iRnd = case testdata of
                        [] => 1
                        | x :: _ => x
        in
           (failedResults, heap_shown, decreased_heap) <- T.unitTest (testdata) ;
           return <xml>
<body><br/>
<p>
         Data1        : {[testdata]}<br/>
         Through Heap : {[heap_shown]}<br/>
         Decreasing first element by 10: {[decreased_heap]}
</p>
<p>Failed tests: <br/> {failedResults}</p>
</body></xml>

end

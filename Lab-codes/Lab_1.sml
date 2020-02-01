fun printInt (a:int) =
print(Int.toString(a)^" ");
fun getInt () = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
fun printIntTable ( [] ) = ()
| printIntTable ( x::xs ) =
let val tmp = printInt(x)
in
printIntTable(xs) end;
fun getIntTable ( 0 ) = []
| getIntTable ( N:int) = getInt()::getIntTable(N-1);
(*****Begin*****) val N = getInt(); val a = getIntTable(N);
fun quickSort [] = []
| quickSort(x::xs) =
letval (left, right) = List.partition(fn y => y < x) xs
in
quickSort(left) @ [x] @ quickSort(right) end
val res = quickSort(a);
printIntTable(res);
(*****End*****)
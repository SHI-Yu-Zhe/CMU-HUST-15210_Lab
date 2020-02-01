fun printInt (a:int) =
print(Int.toString(a)^" ");
fun printIntInf (a:IntInf.int) =
print(IntInf.toString(a)^" ");
fun printReal (a:real) =
print(Real.toString(a)^" ");
fun printString (a:string) =
print(a^" ");
fun getInt () = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
fun getIntInf () = Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) TextIO.stdIn);
fun getReal () = Option.valOf (TextIO.scanStream (Real.scan) TextIO.stdIn);
fun printEndOfLine () =
print("\n");
fun printIntTable ( [] ) = ()
| printIntTable ( x::xs ) =
let val tmp = printInt(x)
in
printIntTable(xs) end;
fun printIntInfTable ( [] ) = ()
| printIntInfTable ( x::xs ) =
let val tmp = printIntInf(x)
in
printIntInfTable(xs) end;
fun getIntTable ( 0 ) = []
| getIntTable ( N:int) = getInt()::getIntTable(N-1);
fun getIntInfTable ( 0 ) = []
| getIntInfTable ( N:int) = getIntInf()::getIntInfTable(N-1);
fun getIntVector ( 0 ) = Vector.fromList []
| getIntVector ( N:int) = Vector.fromList(getIntTable(N));
fun getIntInfVector ( 0 ) = Vector.fromList []
| getIntInfVector ( N:int) = Vector.fromList(getIntInfTable(N));
(*****Begin*****) val N = getInt() and M = getInt(); val list = ListPair.zip(List.tabulate(N, (fn x=>x)), getIntTable(N)); val q = Array2.array(N, N, 0);
fun m i =
letval ss = List.drop(list, i) val tmpHead = Array2.update(q, i, i, #2(hd ss)) val tmpTail = List.app(fn(x, y)=>Array2.update(q, i, x, Int.max(Array2.sub(q, i, x-1), y))) (tl ss)
in
0
end;
List.tabulate(N, m);
fun que _ =
letval left = getInt() - 1 and right = getInt() - 1
in
printInt(Array2.sub(q, left, right)) end;
List.tabulate(M, que);
(*****End*****)
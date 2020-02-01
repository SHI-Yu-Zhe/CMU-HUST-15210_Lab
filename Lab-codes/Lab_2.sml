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
(*****Begin*****) val n = getInt() and m = getInt() and s = getInt() - 1; val maxn = 1000000000; val graph = Array2.array(n, n, maxn);
List.tabulate (n, fn x => Array2.update(graph, x, x, 0));
fun input _ =
letval a = getInt() - 1 and b = getInt() - 1 and c = getInt() val cc = Int.min(c, Array2.sub(graph, a, b)) val ccc = Int.min(c, Array2.sub(graph, b, a)) val t = Array2.update(graph, a, b, cc) val tt = Array2.update(graph, b, a, ccc)
in 0
end;
List.tabulate (m, input); val dsts = Array.array (n, maxn); val v = Array.array (n, false);
Array.update(dsts, s, 0);
fun loose _ =
letval (m, u) = Array.foldli (fn (i, a, (m, u)) => if (not (Array.sub (v, i))) andalso a<m then (a,i) else (m, u)) (maxn, ~1) dsts;
in
if u <> ~1 then
letval t = Array.update(v, u, true) val tt = Vector.foldli (fn (i, a, _) => if a<>maxn andalso a + m < Array.sub(dsts, i) then Array.update(dsts, i, a+m) else ()) ()
(Array2.row(graph, u))
in 0
end
else 0
end;
List.tabulate (n, loose); val output = Array.foldr (op ::) [] dsts;
printIntTable (map (fn x => if x = maxn then ~1 else x)output);
(*****End*****)
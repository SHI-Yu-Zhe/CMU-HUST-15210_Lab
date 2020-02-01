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
printIntInfTable(xs)
end;
fun getIntTable ( 0 ) = []
| getIntTable ( N:int) = getInt()::getIntTable(N-1);
fun getIntInfTable ( 0 ) = []
| getIntInfTable ( N:int) = getIntInf()::getIntInfTable(N-1);
fun getIntVector ( 0 ) = Vector.fromList []
| getIntVector ( N:int) = Vector.fromList(getIntTable(N));
fun getIntInfVector ( 0 ) = Vector.fromList []
| getIntInfVector ( N:int) = Vector.fromList(getIntInfTable(N));
(*****Begin*****) val n = getInt() and m = getInt(); val graph: (int * int) list array = Array.array(n, []);
fun input i =
letval a = getInt() - 1 and b = getInt() - 1
val t = Array.update(graph, a, (b, i)::(Array.sub(graph, a))) val tt = Array.update(graph, b, (a, i)::(Array.sub(graph, b)))
in
0
end;
List.tabulate(m, input); val point = Array.array(n, false); val edge = Array.array(m, false); val dfn = Array.array(n, 0); val low = Array.array(n, 0); val count = Array.array(1, 1);
fun tarjan fa x =
letval num = Array.sub(count, 0) val t = Array.update(count, 0, num+1) val tt = Array.update(dfn, x, num) val ttt = Array.update(low, x, num) val chg = Array.array(1, 0)
fun execute (i, j) =
if (Array.sub (dfn, i)) = 0 then
letval t = Array.update (chg, 0, (Array.sub (chg, 0)) + 1) val tt = tarjan x i val ttt = Array.update(low, x, Int.min(Array.sub (low, x), Array.sub (low, i))) val tttt = if x = 0 andalso (Array.sub (chg, 0)) > 1 then (Array.update (point, x, true)) else () val ttttt = if x <> 0 andalso (Array.sub (dfn, x)) <= (Array.sub(low, i)) then (Array.update (point, x, true)) else () val tttttt = if (Array.sub (dfn, x)) < (Array.sub (low, i)) then (Array.update (edge, j, true)) else ()
in () end
else if i <> fa then Array.update (low, x, Int.min(Array.sub (low, x), Array.sub (dfn, i))) else () val tmp = List.app execute (Array.sub (graph, x))
in () end;
tarjan ~1 0;
printInt (Array.foldl (fn(x, y) => if x then y + 1 else y) 0 point);
printInt (Array.foldl (fn(x, y) => if x then y + 1 else y) 0 edge);
(*****End*****) 
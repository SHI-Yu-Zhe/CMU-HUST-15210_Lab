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
(*****Begin*****)
fun clear [] = [0]
| clear list = if hd list = 0 then clear (tl list) else list; val na = getInt(); val a = List.rev (clear (getIntTable(na))); val nb = getInt(); val b = List.rev (clear (getIntTable(nb)));
fun plus (c, (a, b, [])) = (((a+c) div 10), ((a+c) mod 10)::b, [])
| plus (c, (a, b, x::xs)) = (((a+x+c) div 10), ((a+x+c) mod 10)::b, xs); val (aa, bb, _) = List.foldl plus (0, [], b) a;
if aa>0 then printIntTable(aa::bb) else printIntTable(bb);
print("\n");
fun minus (c, (a, b, [])) =
if c - a < 0 then (1, (10+c-a)::b, []) else (0, (c-a)::b, [])
| minus (c, (a, b, x::xs)) =
if c - x - a < 0 then (1, (10+c-x-a)::b, xs) else (0, (c-x-a)::b, xs); val (_, bb, _) = List.foldl minus (0, [], b) a;
printIntTable (clear bb);
print("\n");
fun multi ((i, x), y) =
let
fun ply (c, (a, b)) = (((a + c * x) div 10), ((a+c*x) mod 10)::b); val (aaa, bbb) = List.foldl ply (0, []) a; val ccc = List.rev((if aaa>0 then aaa::bbb else bbb) @ List.tabulate (i, fn x => 0)); val (aaaa, bbbb, _) = List.foldl plus (0, [], List.rev y) ccc;
in
if aaaa > 0 then aaaa::bbbb else bbbb
end;
printIntTable (clear (List.foldl multi [] (ListPair.zip (List.tabulate (nb, fn x => x), b))));
(*****End*****)
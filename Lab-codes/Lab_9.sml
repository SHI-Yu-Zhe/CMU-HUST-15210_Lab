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
(*****Begin*****) val n = getIntInf(); val table:IntInf.int list = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97];
if (List.exists (fn x => x = n) table) then printString("True") else
let
fun quickPow (x:IntInf.int, exp:IntInf.int, y:IntInf.int) =
case exp of
0 => 1
|1 => x mod y
|otherwise =>
letval z = quickPow (x, exp div 2, y)
in
if exp mod 2 = 0 then z * z mod y else z * z * x mod y end;
fun findTime (x:IntInf.int, cnt) =
if x mod 2 = 0 then findTime (x div 2, cnt+1) else (x, cnt); val (time, k) = findTime (n - 1, 0);
fun test(test:IntInf.int, flag) =
if not flag then false
else
letval t = quickPow(test, time, n);
fun testSec(i, (res, tt:IntInf.int)) =
if not res then (res, tt) else
letval resnext = tt * tt mod n; val rst = not(resnext = 1 andalso tt <> 1 andalso tt <> n - 1);
in
(rst, resnext) end; val (res, at) = List.foldl testSec(true, t) (List.tabulate(k, fn x=>x));
in
res andalso at = 1
end;
in
if List.foldl test true table then printString("True") else printString("False") end;
(*****End*****)
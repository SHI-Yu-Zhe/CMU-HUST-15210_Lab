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
fun printArray ( Arr ) =
let val cur = ref 0
val len = Array.length(Arr)
inwhile !cur < len
do
(
printInt(Array.sub(Arr,!cur)); cur := !cur + 1) end;
fun printString( s ) = print(s ^ " ");
(*function to get sequence in (l,h,r) type*)
fun getSeq(0) = []
| getSeq(N:int) = (getInt(),getInt(),getInt())::getSeq(N-1);
(*fun printSeq([]) = []
| printSeq(x::xs:(int*int) seq) =
let val (t,tt) = (printInt(#1 x), printInt(#2 x))
in printSeq(xs) end;*)
(*****Begin*****) val n = getInt();
fun read _ =
letval a = getInt() and b = getInt() and c = getInt()
in (a, b, c) end; val ra = List.tabulate(n, read);
fun quickSort f [] = []
| quickSort f (x::xs) =
letval left = List.filter(fn y => f (y, x)) xs
val right = List.filter(fn y => not (f (y, x))) xs
in
(quickSort f left) @ [x] @ (quickSort f right) end; val sa = quickSort(fn(a,b) => (#2 a) < (#2 b)) ra; val hdv = Vector.fromList(List.map(fn x => (#2 x)) sa);
val la = ListPair.map (fn (x, (y, _, z)) => (y, x, z)) (List.tabulate(n, fn x=>x), sa);
fun sortedge ([]) = []
| sortedge(x::y:(int*int*int)list) = (#1 x, #2 x, 1)::(#3 x, #2 x, 0)::sortedge(y); val edgea = quickSort (fn (x,y) => (#1 x) < (#1 y)) (sortedge la); val h = Array.array(n, 0); val max = Array.array(1, ~1);
fun skyline(a, b, c) =
if c = 1 then
letval t = Array.update(h, b, (Array.sub(h, b)) + 1) val tmp = Array.sub(max, 0); val tt =
if Array.sub(h, b) = 1 then
if b > tmp then
letval ttt =
if tmp = ~1 orelse (Vector.sub(hdv, b))<>(Vector.sub(hdv, tmp)) then
letval ttttt = printIntTable[a, Vector.sub(hdv, b)] val tttttt = print("\n")
in () end
else () val tttt = Array.update(max, 0, b);
in () end
else () else ()
in () end
else
letval t = Array.update(h, b, (Array.sub(h, b)) - 1) val tt =
if Array.sub(h, b) = 0 then
if b = Array.sub(max, 0) then
let
fun find(~1) = ~1
| find(i) =
if (Array.sub(h, i)) > 0 then i else find(i - 1); val ttt = Array.update(max, 0, find(b)) val tmp = Array.sub(max, 0) val tttt =
if tmp = ~1 then
letval ttttt = printIntTable[a, 0] val tttttt = print("\n")
in () end
else if (Vector.sub(hdv, b)) <> (Vector.sub(hdv, tmp)) then
letval ttttt = printIntTable[a, Vector.sub(hdv, tmp)] val tttttt = print("\n")
in () end
else ()
in () end
else () else ()
in () end;
List.app skyline edgea;
(*****End*****)
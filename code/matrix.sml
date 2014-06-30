type matrix = int list list

(* helper functions *)

fun range(min:int , max: int) = List.tabulate(max - min + 1, (fn x => x + min))

fun even(x: int) = x mod 2 = 0

(* Matrix functions *)

(* sqMatrix(size, value) = a square matrix of size*size, all cells filled with value *)
fun sqMatrix(size: int , value: int) : matrix =
  List.tabulate(size, (fn x => List.tabulate(size, fn y => value)))

(*randomSqMatrix size seed = a square matrix of size*size, 
    cells filled with random numbers coming from the seed *)
fun randomSqMatrix(size: int, seed: Random.rand) : matrix =
  map (fn r => map (fn x => Random.randRange(0, 10) seed) r) 
      (sqMatrix(size, 0))

(* getMinor m (i,j) = the minor of the matrix m for (i,j) *)
fun getMinor (m: matrix) (i: int , j: int) : matrix =
  let
    fun without(xs, n) =
      (#2 (ListPair.unzip 
        (List.filter (fn (i, e) => i <> n) 
                    (ListPair.zip(range(1, length xs), xs)) )))
  in
    map (fn r => without(r, j)) (without(m, i))
  end

(* getFirstRowPairs m = the list of (row, col) for the first row of the matrix *)
fun getFirstRowPairs(m : matrix) : (int * int) list =
  ListPair.zip (List.tabulate(length m, fn x => 1), range(1, length m))

(* getCell m (i, j) = the cell in the matrix m in (i,j) *)
fun getCell (m: matrix) (i: int, j: int) : int =
  List.nth(List.nth(m, i - 1), j - 1)

(* evensNegative xs = the same list with the elements with even indices negated
*                     indices start from 1 *)

fun evensNegative(xs : int list) : int list =
  map (fn (f, s) => if f then ~s else s)
  (ListPair.zip((map even (range(1, length xs))), xs))

(* getDeterminant m = calculates determinant for matrix m *)
fun getDeterminant(m: matrix) : int =
  if length m = 1
  then getCell m (1, 1)
  else 
    foldr (fn (x,y) => x + y) 0
    (evensNegative(map (fn p => (getCell m p) * (getDeterminant (getMinor m p)))
                       (getFirstRowPairs m)))

fun toString(m: matrix) : string =
  "[" ^ (String.concatWith ", " (map (fn r => "[" ^ (String.concatWith ", " (map
  Int.toString r)) ^ "]") m)) ^ "]"

(* IO function *)
fun main() : unit =
  let
    val () = print "Specify a size for your n * n matrix:\n"
    val (SOME input) = TextIO.inputLine TextIO.stdIn
    val (SOME n)     = Int.fromString(input)
    val m = randomSqMatrix(n, Random.rand(n*3, n*20))
    val () = print "Random Matrix:\n"
    val () = print(toString m ^ "\n")
    val () = print "Determinant of the matrix above:\n"
    val () = print(Int.toString(getDeterminant m) ^ "\n\n")
  in
    main()
  end;

main();

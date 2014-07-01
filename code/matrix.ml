(* OCaml version *)

type matrix = int list list

(* helper functions *)

  (* zip from 
    http://www.matt-mcdonnell.com/code/code_ocaml/ocaml_fold/ocaml_fold.html *)
  (* Zip two lists (possibly unequal lengths) into a tuple *)
  let rec zip lst1 lst2 = match lst1,lst2 with
    | [],_ -> []
    | _, []-> []
    | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)

  (* unzip from http://www.cl.cam.ac.uk/~jrh13/atp/OCaml/lib.ml *)
  (* Unzip a list of tuples to two lists *)
  let rec unzip l =
    match l with
      [] -> [],[]
    | (x,y)::t ->
        let xs,ys = unzip t in x::xs,y::ys

  (* OCaml equivalent of Standard ML's List.tabulate *)
  let rec tabulate n f = let rec aux x = (if x == n then [] else (f x)::(aux (x+1))) in aux 0

  (* range(i,j) = [i, (i+1), .., j] *)
  let rec range (i: int) (j: int) = if i > j then [] else i :: range (i+1) j

  let even (x: int) = x mod 2 = 0

(* Matrix functions *)

  (* sqMatrix size value = a square matrix of size*size, all cells filled with value *)
  let sqMatrix (size: int) (value: int) : matrix =
    tabulate size (fun x -> tabulate size (fun y -> value))

  (* randomSqMatrix size = a square matrix of size*size,
  *                       filled with random numbers *)
  let randomSqMatrix (size: int) : matrix =
    List.map (fun r -> List.map (fun x -> Random.int 20) r) 
        (sqMatrix size 0)

  (* getMinor m (i,j) = the minor of the matrix m for (i,j) *)
  let getMinor (m: matrix) ((i: int), (j: int)) : matrix =
    let without xs n =
      snd (unzip 
        (List.filter (fun (i, e) -> i <> n) 
                    (zip (range 1 (List.length xs)) xs)) )
    in
      List.map (fun r -> without r j) (without m i)

  (* getFirstRowPairs m = the list of (row, col) for the first row of the matrix *)
  let getFirstRowPairs(m : matrix) : (int * int) list =
    zip (tabulate (List.length m) (fun x -> 1)) (range 1 (List.length m))

  (* getCell m (i, j) = the cell in the matrix m in (i,j) *)
  let getCell (m: matrix) ((i: int), (j: int)) : int =
    List.nth (List.nth m (i - 1)) (j - 1)

  (* evensNegative xs = the same list with the elements with even indices negated
  *                     indices start from 1 *)
  let evensNegative(xs : int list) : int list =
    List.map (fun (f, s) -> if f then -s else s)
    (zip (List.map even (range 1 (List.length xs))) xs)


  (* getDeterminant m = calculates determinant for matrix m *)
  let rec getDeterminant(m: matrix) : int =
    if List.length m == 1
    then getCell m (1, 1)
    else 
      List.fold_right (fun x y -> x + y)
      (evensNegative(List.map (fun p -> (getCell m p) * (getDeterminant (getMinor m p)))
                        (getFirstRowPairs m))) 0

  let toString(m: matrix) : string =
    "[" ^ (String.concat ", " (List.map (fun r -> "[" ^ (String.concat ", " (List.map
    string_of_int r)) ^ "]") m)) ^ "]"

(* IO function *)
  let rec main() : unit =
    let () = Random.self_init() in
    let () = print_string "Specify a size for your n * n matrix:\n" in
    let n = int_of_string (read_line()) in
    let m = randomSqMatrix n in
    let () = print_string "Random Matrix:\n" in
    let () = print_string (toString m ^ "\n") in
    let () = print_string "Determinant of the matrix above:\n" in
    let () = print_string ((string_of_int (getDeterminant m)) ^ "\n\n") in
    main();;

  main()

// Scala version

object Matrix {
  type Matrix = List[List[Int]]

  // sqMatrix(size, value) = a square matrix of size*size, all cells filled with value
  def sqMatrix(size: Int, value: Int) : Matrix =
    List.fill(size)(List.fill(size)(value))

  // randomSqMatrix(size, limit) = a square matrix of size*size,
  //                 cells filled with random numbers up to limit
  def randomSqMatrix(size: Int, limit: Int) : Matrix =
    List.tabulate(size)(n => List.fill(size)(util.Random.nextInt(limit)))

  // getMinor(m)(i, j) = the minor of the matrix m for (i,j)
  def getMinor(m: Matrix)(i: Int, j: Int) : Matrix = {
    // without(xs, n) = xs without nth element (n starting from 1)
    def without[A](xs: List[A], n: Int) : List[A] =
      xs.zipWithIndex.filterNot { case (_, i) => i + 1 == n}.unzip._1

    without(m, i).map(r => without(r, j))
  }

  // getFirstRowPairs(m) = the list of (row, col) for the first row of the matrix
  def getFirstRowPairs(m: Matrix) : List[(Int, Int)] =
    (1 to (m.length)).toList.map(y => (1, y))

  // getCell(m)(i, j) = the cell in the matrix m in (i,j)
  def getCell(m: Matrix)(i: Int, j: Int) : Int =
    m(i - 1)(j - 1)

  // evensNegative(xs) = the same list with the elements with even indices negated
  //                    indices start from 1
  def evensNegative(xs: List[Int]) : List[Int] =
    xs.zipWithIndex
      .map { case (x, n) => (x, n % 2 != 0) }
      .map { case (x, isEven) => if (isEven) -x else x }

  // getDeterminant(m) = calculates determinant for matrix m
  def getDeterminant(m: Matrix) : Int =
    if (m.length == 1) getCell(m)(1, 1)
    else {
      val list = getFirstRowPairs(m).map(p =>
          (getCell(m) _).tupled(p) * getDeterminant((getMinor(m) _).tupled(p)))
      evensNegative(list).sum
    }

  // toString(m) = string representation of m
  def toString(m: Matrix) : String = {
    def listToString[A](l : List[A]) : String =
      "[" + l.mkString(",") + "]"

    listToString(m.map(listToString))
  }

  // IO Function
  // main = asks for user input to create a matrix and calculate its determinant
  def main(args: Array[String]) {
    println("Specify a size for your n * n matrix:")
    val size = scala.io.StdIn.readInt
    val m = randomSqMatrix(size, 50)
    println("Random Matrix:")
    println(toString(m))
    println("Determinant of the matrix above:")
    println(getDeterminant(m))
    println("---------------------\n")
    main(Array());
  }
}

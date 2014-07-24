// Swift version
import Foundation

typealias Matrix = [[Int]]

// Helper function

  // range(min, max) = [min, min + 1, .., max]
  func range(min: Int, max: Int) -> [Int] {
    return (min...max).map({$0})
  }

  // zip and unzip borrowed from https://gist.github.com/kristopherjohnson/6a76b7e03fc7a8d6a918
  // zip(a1, a2) = a list that has zipped elements of the arguments it takes
  func zip<T, U>(a1: Array<T>, a2: Array<U>) -> Array<(T, U)> {
      assert(a1.count == a2.count);
      let count = a1.count
      var result = Array<(T, U)>()
      for i in 0..<count {
          let newElem = (a1[i], a2[i])
          result.append(newElem)
      }
      return result
  }

  // unzip(a) =  a list that has two list,
  //             the first one has the first elements of l,
  //             the second has the second elements of l,
  //             only works for dual zip
  func unzip<T, U>(a: Array<(T, U)>) -> (Array<T>, Array<U>) {
      let count = a.count
      var aT = Array<T>()
      var aU = Array<U>()
      for (t, u) in a {
          aT.append(t)
          aU.append(u)
      }
      return (aT, aU)
  }

  // without(xs, n) = xs without nth element (n starting from 1)
  func without<T>(xs: Array<T>, n: Int) -> Array<T> {
    return unzip(zip(range(1, xs.count), xs)
                    .filter({(i, _) in i != n})).1
  }

// Matrix functions

  // sqMatrix(size, value) = a square matrix of size*size, all cells filled with value
  func sqMatrix(size: Int, value: Int) -> Matrix {
    return [[Int]](count: size, repeatedValue: [Int](count: size, repeatedValue: value))
  }

  // randomSqMatrix(size, limit) = a square matrix of size*size,
  //                 cells filled with random numbers up to limit
  func randomSqMatrix(size: Int, limit: UInt32) -> Matrix {
    return sqMatrix(size, 0)
            .map({r in r.map({
                  _ in Int(arc4random_uniform(limit))
                }) })
  }

  // getMinor(m)(i, j) = the minor of the matrix m for (i,j)
  func getMinor(m: Matrix, i: Int, j: Int) -> Matrix {
    return without(m, i).map({r in without(r, j)})
  }

  // getFirstRowPairs(m) = the list of (row, col) for the first row of the matrix
  func getFirstRowPairs(m: Matrix) -> [(Int, Int)] {
    return range(1, m.count).map({y in (1, y)})
  }

  // getCell(m)(i, j) = the cell in the matrix m in (i,j)
  func getCell(m: Matrix, i: Int, j: Int) -> Int {
    return m[i - 1][j - 1]
  }

  // evensNegative(xs) = the same list with the elements with even indices negated
  //                    indices start from 1
  func evensNegative(xs: [Int]) -> [Int] {
    return zip(range(1, xs.count), xs)
            .map({(n, x) in (n % 2 == 0, x)})
            .map({(isEven, x) in isEven ? -x : x})
  }

  // getDeterminant(m) = calculates determinant for matrix m
  func getDeterminant(m: Matrix) -> Int {
    if m.count == 1 {
      return getCell(m, 1, 1)
    } else {
      let list = getFirstRowPairs(m)
        .map({getCell(m, $0, $1) * getDeterminant(getMinor(m, $0, $1))})
      return evensNegative(list).reduce(0, +)
    }
  }

  // IO Function

  // input() = gets input from user
  // borrowed from http://stackoverflow.com/a/24021467/2016295
  func input() -> String {
      var keyboard = NSFileHandle.fileHandleWithStandardInput()
      var inputData = keyboard.availableData
      var gotten = NSString(data: inputData, encoding:NSUTF8StringEncoding)
      return gotten.stringByTrimmingCharactersInSet(NSCharacterSet.newlineCharacterSet())
  }

  // main = asks for user input to create a matrix and calculate its determinant
  func main() {
    println("Specify a size for your n * n matrix:")
    var gotten = input()
    if let size = gotten.toInt() {
      var m = randomSqMatrix(size, 50)
      println("Random Matrix:")
      println(m)
      println("Determinant of the matrix above:")
      println(getDeterminant(m))
    } else {
      println("Not a valid size, try again.")
    }
    println("---------------------\n")
    main()
  }
  main()

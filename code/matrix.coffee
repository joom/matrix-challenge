# CoffeeScript version (runs with Node.js)
# written as functional as possible
(->
  # Helper functions

  # ask(q, cb) = prompts a question to user, calls callback with answer
  ask = (question, callback) ->
    r = require("readline").createInterface(
      input: process.stdin
      output: process.stdout
    )
    r.question question + "\n", (answer) ->
      r.close()
      callback null, answer

  # getRandomInt(min, max) = random number between min and max (inclusive)
  getRandomInt = (min, max) ->
    Math.floor(Math.random() * (max - min + 1)) + min

  # Matrix functions

  # sqMatrix(size, value) = a square matrix of size*size, all cells filled with value
  sqMatrix = (size, val) ->
    row = Array.apply(null, new Array(size)).map -> val
    Array.apply(null, new Array(size)).map -> row

  # randomSqMatrix(size, limit) = a square matrix of size*size,
  #                 cells filled with random numbers up to limit
  randomSqMatrix = (size, limit) ->
    sqMatrix(size, 0).map (row) ->
      row.map -> getRandomInt 0, limit

  # getMinor(m, i, j) = the minor of the matrix m for (i,j)
  getMinor = (matrix, i, j) ->
    matrix.filter((row, n) ->
      i isnt (n + 1)
    ).map (row) ->
      row.filter (cell, n) ->
        j isnt (n + 1)

  # getFirstRowPairs(m) = the list of [row, col] for the first row of the matrix
  getFirstRowPairs = (matrix) ->
    Array.apply(null, new Array(matrix.length)).map (e, n) ->
      [ 1, n + 1]

  # getCell(m, i, j) = the cell in the matrix m in (i,j)
  getCell = (matrix, i, j) -> matrix[i - 1][j - 1]

  # evensNegative(xs) = the same list with the elements with even indices negated
  #                    indices start from 1
  evensNegative = (list) ->
    list.map (x, n) ->
      (if ((n + 1) % 2 is 0) then -x else x)

  # getDeterminant(m) = calculates determinant for matrix m
  getDeterminant = (matrix) ->
    if matrix.length is 1
      getCell matrix, 1, 1
    else
      list = getFirstRowPairs(matrix).map((p) ->
        getCell(matrix, p[0], p[1]) * getDeterminant(getMinor(matrix, p[0], p[1]))
      )
      evensNegative(list).reduce (p, c) -> p + c

  # IO function
  main = ->
    ask "Specify a size for your n * n matrix:", (err, n) ->
      m = randomSqMatrix(parseInt(n), 20)
      console.log "Random Matrix:"
      console.log JSON.stringify(m)
      console.log "\nDeterminant of the matrix above:"
      console.log getDeterminant(m) + "\n-----\n"
      main()

  main()
)()

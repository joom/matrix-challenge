// JavaScript version (runs with Node.js)
// written as functional as possible

(function() {
  // Helper functions

  // ask(q, cb) = prompts a question to user, calls callback with answer
  var ask = function(question, callback) {
    var r = require('readline').createInterface({
      input: process.stdin,
      output: process.stdout
    });
    r.question(question + '\n', function(answer) {
      r.close();
      callback(null, answer);
    });
  };

  // getRandomInt(min, max) = random number between min and max (inclusive)
  var getRandomInt = function(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
  };

  // Matrix functions

  // sqMatrix(size, value) = a square matrix of size*size, all cells filled with value
  var sqMatrix = function(size, val) {
    var row = Array.apply(null, new Array(size)).map(function(){
      return val;
    });
    return Array.apply(null, new Array(size)).map(function(){
      return row;
    });
  };

  // randomSqMatrix(size, limit) = a square matrix of size*size,
  //                 cells filled with random numbers up to limit
  var randomSqMatrix = function(size, limit) {
    return sqMatrix(size, 0).map(function (row) {
      return row.map(function () {
        return getRandomInt(0, limit);
      });
    });
  };

  // getMinor(m, i, j) = the minor of the matrix m for (i,j)
  var getMinor = function(matrix, i, j) {
    return matrix.filter(function (row, n) {
      return i !== (n + 1);
    }).map(function (row) {
      return row.filter(function (cell, n) {
        return j !== (n + 1);
      });
    });
  };

  // getFirstRowPairs(m) = the list of [row, col] for the first row of the matrix
  var getFirstRowPairs = function (matrix) {
    return Array.apply(null, new Array(matrix.length)).map(function(e, n){
      return [1, n + 1];
    });
  };

  // getCell(m, i, j) = the cell in the matrix m in (i,j)
  var getCell = function (matrix, i, j) {
    return matrix[i - 1][j - 1];
  };

  // evensNegative(xs) = the same list with the elements with even indices negated
  //                    indices start from 1
  var evensNegative = function (list) {
    return list.map(function (x, n) {
      return ((n + 1) % 2 === 0) ? -x : x;
    });
  };

  // getDeterminant(m) = calculates determinant for matrix m
  var getDeterminant = function (matrix) {
    if(matrix.length === 1) {
      return getCell(matrix, 1, 1);
    } else {
      var list = getFirstRowPairs(matrix).map(function (p) {
        return getCell(matrix, p[0], p[1]) * getDeterminant(getMinor(matrix, p[0], p[1]));
      });
      return evensNegative(list).reduce(function (p, c) {
        return p + c;
      });
    }
  };

  // IO function
  var main = function() {
    ask("Specify a size for your n * n matrix:", function(err, n) {
      var m = randomSqMatrix(parseInt(n), 20);
      console.log("Random Matrix:");
      console.log(JSON.stringify(m));
      console.log("\nDeterminant of the matrix above:");
      console.log(getDeterminant(m) + "\n-----\n");
      main();
    });
  };

  main();
})();

-- Haskell version

import System.Random
import Data.List.Split (chunksOf)

type Matrix = [[Int]]
data Parity = Even | Odd deriving (Show, Eq)

--sqMatrix size val = a square matrix of size*size, all cells filled with val
sqMatrix :: Int -> Int -> Matrix
sqMatrix size val = take size $ repeat (take size [val,val..val])

--randomSqMatrix size seed = a square matrix of size*size,
--  cells filled with random numbers coming from the seed
randomSqMatrix :: (RandomGen g) => Int -> g -> [[Int]]
randomSqMatrix size seed = chunksOf size rndMod10Seq
    where rndSeq = (take (size^2) (randoms seed :: [Int]))
          rndMod10Seq = map (\a -> a `mod` 10) rndSeq

--getMinor m (i,j) = the minor of the matrix m for (i,j)
getMinor :: Matrix -> (Int, Int) -> Matrix
getMinor m (i, j) = map (\r -> without r j) $ without m i
    where size = length m
          without xs n = snd $ unzip $ filter (\(i, e) -> i/=n) (zip [1..] xs)

--getFirstRowPairs m = the list of (row, col) for the first row of the matrix
getFirstRowPairs :: Matrix -> [(Int, Int)]
getFirstRowPairs m = zip [1,1..1] [1..(length m)]

--getCell m (i, j) = the cell in the matrix m in (i,j)
getCell :: Matrix -> (Int, Int) -> Int
getCell m (i, j) = m !! (i - 1) !! (j - 1)

--evensNegative xs = the same list with the elements with even indices negated
--  indices start from 1
evensNegative :: Num b => [b] -> [b]
evensNegative xs =
    map (\(p,x) -> if p == Even then -x else x) $ zip (cycle [Odd,Even]) xs

--getDeterminant m = calculates determinant for matrix m
getDeterminant :: Matrix -> Int
getDeterminant m
    | length m == 1 = getCell m (1,1)
    | otherwise     = sum $ evensNegative list
        where list = map (\p -> (getCell m p) * (getDeterminant (getMinor m p)) ) (getFirstRowPairs m)

--main = asks for user input to create a matrix and calculate its determinant
main :: IO ()
main = do
    putStrLn "Specify a size for your n * n matrix:"
    input <- getLine
    let size = read input :: Int
    seed  <- newStdGen
    let rs = randomSqMatrix size seed
    putStrLn "Random Matrix:"
    putStrLn $ show rs
    putStrLn "Determinant of the matrix above:"
    putStrLn $ show $ getDeterminant rs

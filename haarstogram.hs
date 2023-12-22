import Data.List (transpose)

main :: IO ()
main = print $ solve 2 21 [
  [8, 3, -2, 8],
  [3, 7, -2, 0],
  [3, 8,  9, 3],
  [1, 9,  9, 3] ]

-- Count the number submatrices of size d with a given target sum.
solve :: Int -> Int -> [[Int]] -> Int
solve d target m = sum $ map (length . filter (== target)) $ reduce d m

-- Collapse a matrix both horizontally and then verically into sums.
reduce :: Int -> [[Int]] -> [[Int]]
reduce d m = map (collapse d) (transpose (map (collapse d) m))

-- Collapse a list into sums of length d of consequtive integers.
collapse :: Int -> [Int] -> [Int]
collapse d l = rollSum (sum (take d l)) l (drop d l)

-- Given a total, previous list of ints and next list, produce the new sums.
rollSum :: Int -> [Int] -> [Int] -> [Int]
rollSum total prev [] = [total]
rollSum total (p:prev) (n:next) = total : rollSum (total-p+n) prev next

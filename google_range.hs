import Test.HUnit
import Data.Ord
import Data.List
import Data.Function

{-
Reduce a list of ranges to a minimal list of ranges, where overlaps are
combined into one range.
-}

main = runTestTT $ TestList [
    "test 1" ~: reduceRanges [(2,3),(9,10),(1,5),(4,7)] ~?= [(1,7),(9,10)]
  , "empty" ~: reduceRanges [] ~?= []
  , "adjacent" ~: reduceRanges [(5,9),(600,800),(1,4)] ~?= [(1,9),(600,800)]
  ]

reduceRanges :: [(Int,Int)] -> [(Int,Int)]
reduceRanges = consolidate . sortBy (comparing fst)

consolidate :: [(Int,Int)] -> [(Int,Int)]
consolidate [] = []
consolidate [pair] = [pair]
consolidate ((s1,e1):(s2,e2):rest)
  | e1 + 1 >= s2 = consolidate ((s1,e2):rest)
  | otherwise = (s1,e1):consolidate ((s2,e2):rest)

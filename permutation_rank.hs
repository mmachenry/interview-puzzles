import Data.List

permutationRank str = elemIndex str $ sort $ permutations str


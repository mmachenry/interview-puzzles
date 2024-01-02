import Data.List

main = mapM_ print (queens 8)

queens n = [ res |
    let ranks = [1..n],
    ys <- permutations ranks,
    let res = zip ranks ys,
    not_attacking_rest res]

not_attacking_rest [] = True
not_attacking_rest (queen:rest) =
    all (not_attacking queen) rest
    && not_attacking_rest rest

not_attacking (x1,y1) (x2,y2) = abs(x1-x2) /= abs(y1-y2)
    

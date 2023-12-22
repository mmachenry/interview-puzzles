import qualified Data.Vector as V

data Direction = R | L

solve dirs = foldl foldOnce (V.generate (2^(length dirs)) (\i->[i+1])) dirs

foldOnce paper dir =
    let l = V.length paper
        mid = l `quot` 2
    in V.generate mid (\i->
        let (top,bot) = case dir of L -> (mid-i-1,mid+i); R -> (l-i-1,i)
        in reverse (paper V.! top) ++ paper V.! bot)

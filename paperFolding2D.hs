import qualified Data.Vector as V

data Direction = R | L | T | B deriving Eq

solve dirs = foldl foldOnce (initialPaper (2^((length dirs) `quot` 2))) dirs

initialPaper n = generate2D (n,n) (\(i,j)->[i*n + j `mod` n + 1])

index2D v (i,j) = (v V.! i) V.! j

generate2D (len,wid) f = V.generate len (\i->V.generate wid (\j->f (i,j)))

combine paper (u,l) = reverse (index2D paper u) ++ (index2D paper l)

foldOnce paper dir =
    let len = V.length paper
        wid = V.length (paper V.! 0)
        l = if dir == T || dir == B then len `quot` 2 else len
        w = if dir == L || dir == R then wid `quot` 2 else wid
        coords (i,j) = case dir of
          R -> ((i,wid-j-1), (i, j))
          L -> ((i,w-j-1), (i,w+j))
          T -> ((l-i-1,j), (l+i,j))
          B -> ((len-i-1,j), (i,j))
    in generate2D (l,w) ((combine paper) . coords)

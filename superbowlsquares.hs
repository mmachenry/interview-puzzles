import Data.Function.Memoize
import Data.List
import Data.Ord

main = putStrLn $ show $ solution 200

memoP = memoize2 p
p :: Int -> Int -> Float
p 1 0 = 0.5
p 1 1 = 0.3
p 1 2 = 0.2
p 1 _ = 0.0
p r s =
      memoP 1 0 * memoP (r-1) s
    + memoP 1 1 * memoP (r-1) (s-1)
    + memoP 1 2 * memoP (r-1) (s-2)

q r x y = memoP r x * memoP r y

solution r = take 10 $ sortBy (flip (comparing snd)) $ results
    where results = [ ((x,y), q r x y) | x <- [0..(2*r)], y <- [0..x]]


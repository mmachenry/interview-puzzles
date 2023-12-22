-- Given to me by Presley, used at Originate

import Data.List
import Test.HUnit

type Point = (Float, Float)
px = fst
py = snd

solve :: [[Point]] -> [[Point]]
solve points =
    let allX = nub $ sort $ concatMap (map px) points
    in map (fillIn allX) points

fillIn [] ps = ps
fillIn xs [] = undefined
fillIn (x:xs) [p1]
    | x == px p1 = fillIn xs [p1]
    | x < px p1 = (x,0) : fillIn xs [p1]
    | otherwise = p1 : map (\x->(x,py p1)) (x:xs)
fillIn (x:xs) (p1:p2:ps)
    | x < px p1 = (x,0) : fillIn xs (p1:p2:ps)
    | x == px p1 = fillIn xs (p1:p2:ps)
    | x > px p1 && x < px p2 =
        p1 : (x, linearInterpolate p1 p2 x) : fillIn xs (p2:ps)
    | x > px p1 = p1 : fillIn (x:xs) (p2:ps)

linearInterpolate :: Point -> Point -> Float -> Float
linearInterpolate (x1,y1) (x2,y2) x =
    let slope = (y2 - y1) / (x2 - x1)
        run = x - x1
    in slope * run + y1

main = runTestTT $ TestList [
    solve [[(1,4), (3, 12)], [(1, 2), (2, 4)]] ~?= [
        [(1,4),(2,8),(3,12)],
        [(1,2),(2,4),(3,4)]
        ]
    , solve [[(1,4),(3,12)],[(2,2)]] ~?= [
        [(1,4),(2,8),(3,12)],
        [(1,0),(2,2),(3,2)]
        ]
    ]


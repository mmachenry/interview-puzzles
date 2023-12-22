import Data.List
import Data.Ord

type Posn = (Int, Int)
type Face = Int
data Die = Die {
    diePosn :: Posn,
    dieTop :: Face,
    dieFront :: Face,
    dieLeft :: Face,
    dieScores :: [Face]
    } deriving (Show)
data Direction = North | South | East | West

move :: Die -> Direction -> Die
move (Die (x,y) top front left scores) dir = case dir of
    North -> Die (x,y+1) (opposite front) top left (top:scores)
    South -> Die (x,y-1) front (opposite top) left (top:scores)
    East -> Die (x+1,y) left front (opposite top) (top:scores)
    West -> Die (x-1,y) (opposite left) front top (top:scores)

opposite :: Int -> Int
opposite f = 7 - f

scores :: Posn -> Die -> [Die]
scores target die
    | diePosn die == target = [die]
    | otherwise = do
        legal <- moves (diePosn die) target
        map (move die) (moves (diePosn die) target) >>= scores target

moves :: Posn -> Posn -> [Direction]
moves (px,py) (tx, ty) =
    if px < tx then [East] else []
    ++ if py < ty then [North] else []

initDie :: Die
initDie = Die (0,0) 1 2 3 []

solution = minimumBy (comparing dieScore) $ scores (3,2) initDie
    where dieScore die = dieTop die + sum (dieScores die)


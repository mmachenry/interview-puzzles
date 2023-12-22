import Data.List
import Data.Ord
import Data.Maybe

data Color = R | G | B deriving (Show, Eq)
type GameState = (Board, Int)
type Bubble = Maybe Color
type Board = [[Bubble]]
type Move = (Int, Int)

solve :: GameState -> GameState
solve = maximumBy (comparing snd) . possibleEndStates

possibleEndStates :: GameState -> [GameState]
possibleEndStates gs =
    let moves = validMoves gs
    in if null moves
       then [gs]
       else concatMap possibleEndStates $ map (makeMove gs) moves

validMoves :: GameState -> [Move]
validMoves (board,_) =
    concat $ zipWith (\c rs->map (\r->(c,r)) [0..length rs - 1]) [0..] board

makeMove :: GameState -> Move -> GameState
makeMove (board, score) position =
    let newBoard = popAdjacent board position
        score' = 2 ^ countPopped newBoard
    in (gravitate newBoard, score+score')

popAdjacent :: Board -> Move -> Board
popAdjacent board (x,y) =
    foldl' (boardMod (pop (board!!x!!y))) board adjacentPosns
    where adjacentPosns = [(x,y),(x-1,y),(x+1,y),(x,y+1),(x,y-1)]

boardMod :: (Bubble -> Bubble) -> Board -> Move -> Board
boardMod f board (x,y) = listMod x (listMod y f) board
    where listMod :: Int -> (a -> a) -> [a] -> [a]
          listMod 0 g (b:bs) = g b : bs
          listMod n g (b:bs) = b : listMod (n-1) g bs
          listMod _ _ [] = []

pop :: Bubble -> Bubble -> Bubble
pop (Just color) (Just c) = if color == c then Nothing else Just c
pop _ x = x

gravitate :: Board -> Board
gravitate = map $ filter isJust

countPopped :: Board -> Int
countPopped board = sum (map (length . filter isNothing) board)

testBoard :: GameState
testBoard = (map (map Just) [[B, R, R], [B, R, G], [B, B, G]], 0)


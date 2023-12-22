import Data.List
import Data.Ord
import Data.Matrix

data Bubble = R | G | B | X deriving (Show, Eq)
type GameState = (Board, Int)
type Board = Matrix Bubble
type Move = (Int, Int)

solve :: GameState -> GameState
solve = maximumBy (comparing snd) . possibleEndStates

possibleEndStates :: GameState -> [GameState]
possibleEndStates gs@(board,score) =
    let moves = validMoves board 
    in if null moves
       then [gs]
       else concatMap possibleEndStates $ map (makeMove gs) moves

validMoves :: Board -> [Move]
validMoves board = [ (x,y) |
    x <- [1 .. ncols board],
    y <- [1 .. nrows board],
    getElem x y board /= X ]

makeMove :: GameState -> Move -> GameState
makeMove (board, score) position =
    let newBoard = popAdjacent board position
        score' = 2 ^ (countPopped newBoard - countPopped board)
    in (gravitate newBoard, score+score')

popAdjacent :: Board -> Move -> Board
popAdjacent board (x,y) = foldl' (flip (setElem X)) board [ (x', y') |
    (x', y') <- [(x,y),(x-1,y),(x+1,y),(x,y+1),(x,y-1)],
    x' > 0 && x' <= ncols board && y' > 0 && y' <= nrows board,
    getElem x' y' board == getElem x y board
    ]

gravitate :: Board -> Board
gravitate = fromLists . map (sortBy (comparing bubbleToBinary)) . toLists

countPopped :: Board -> Int
countPopped board = sum $ fmap bubbleToBinary board

bubbleToBinary bubble = if bubble == X then 1 else 0

testGameState :: GameState
testGameState = (fromLists [[R, G, G], [R, R, B], [B, B, B]], 0)


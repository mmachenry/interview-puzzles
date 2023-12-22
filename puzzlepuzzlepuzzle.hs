import Data.Maybe

type Id = Maybe Int
newtype Piece = Piece [Id]

testPuzzle1 = [
  Piece [Just 1, Nothing, Nothing, Just 2],
  Piece [Nothing, Nothing, Just 1, Just 3],
  Piece [Just 4, Just 2, Nothing, Nothing],
  Piece [Nothing, Just 3, Just 4, Nothing]
  ]

isCorner (Piece [e, n, w, s]) = length (filter isNothing [e,n,w,s]) == 2

-- find corner, rotate to top left
-- go left and create a row
-- check that this row's top matches the previous row (if any's bottom)
-- if bottom is null then return if the box is empty
-- if not recur

isPuzzleFirst :: [Piece] -> Bool
isPuzzleFirst box =
    case find isCorner box of
        Nothing -> False
        Just aCorner ->
            let topLeft = rotateTopLeft aCorner
                (topRow, remainingBox) = buildRow topLeft box
            in -- TODO guard top is edge
               isPuzzle remainingBox topRow

isPuzzle :: [Piece] -> [Piece] -> Bool
isPuzzle box prev =
    let topId = south (head prev)
    case find (has topId) box of
        Nothing -> False
        Just edge ->
            let left = rotate edge
                (row, remainingBox) = buildRow left box
            in guard top edge matches
               if not bottom edge
               then isPuzzle remainingBox row

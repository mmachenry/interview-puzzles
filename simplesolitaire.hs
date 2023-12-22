import Control.Parallel
import Control.Monad.Random
import Data.List
import Data.Ord
import System.Random
import qualified Data.Map as Map
import Control.Monad (forM_)
import Control.Applicative
import qualified Data.Sequence as S
import Data.Traversable
import Data.Maybe
import Data.Monoid
import Data.Function

import Debug.Trace

import Test.HUnit
import Test.QuickCheck

data GameState = GameState [Card] [Card] Piles
type Piles = Map.Map Int Rank
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq,Show,Bounded,Enum)
type Rank = Int
data Card = Card Rank Suit deriving (Eq, Show)
type Strategy = [Card] -> Piles -> Card -> Maybe Int

numPiles = 10
numRanks = 13
simulationRuns = 1000

strategies = [
    ("alwaysDiscard", alwaysDiscard),
    ("place1to10", place1to10),
    ("placeScaledRange", placeScaledRange),
    ("placeScaledAvailableRange", placeScaledAvailableRange),
    ("placeMedianPossible", placeMedianPossible),
    ("placeClosestDiscardRatio", placeClosestDiscardRatio),
    ("placeScaledRangeOrStepToTheSide", placeScaledRangeOrStepToTheSide)
    ]

main = forM_ strategies $ \(name, strategy)-> do
    wins <- evalRandIO $ numWins strategy simulationRuns
    putStrLn $ name ++ " " ++ show wins ++ "/" ++ show simulationRuns

numWins :: RandomGen g => Strategy -> Int -> Rand g Int
numWins strategy gamesToPlay = do
    results <- S.replicateM gamesToPlay $ do
        deck <- shuffle startingDeck
        return $ simulateGame strategy $ initState deck
    return $ S.length $ S.filter id results

startingDeck = [ Card r s | r <- [1..numRanks], s <- [Clubs ..] ]

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle l = do
    randNums <- getRandoms
    return $ map snd $ sortBy (comparing fst) $ zip (randNums::[Int]) l

simulateGame :: Strategy -> GameState -> Bool
simulateGame strategy state@(GameState (topCard:deck) discard piles) =
    --trace (show state) $
    isWin state || (not (isLoss state) &&
    simulateGame strategy (case strategy discard piles topCard of
        Nothing -> discardTop state
        Just spot -> placeTop state spot))

initState deck = GameState deck [] Map.empty
isWin (GameState _ _ piles) = Map.size piles == numPiles
isLoss (GameState _ discardPile _) = length discardPile >= 6
discardTop (GameState (topCard:drawPile) discardPile slots) =
    GameState drawPile (topCard:discardPile) slots
placeTop gs@(GameState (Card rank _:drawPile) discardPile piles) index =
    if index >= 0 && index < numPiles
       && fromMaybe rank (Map.lookup index piles) == rank
    then GameState drawPile discardPile (Map.insert index rank piles)
    else error $ "Bad index: " ++ show gs ++ " " ++ show index

----------------------------------------------------------------------
-- Strategies
----------------------------------------------------------------------
alwaysDiscard :: Strategy
alwaysDiscard _ _ _ = Nothing

place1to10 :: Strategy
place1to10 _ _ (Card rank _) =
    if rank >= 1 && rank <= numPiles
    then Just (rank-1)
    else Nothing

placeScaledRange :: Strategy
placeScaledRange _ piles (Card rank _) =
    let i = scaledIndex numRanks numPiles rank
    in case Map.lookup i piles of
           Nothing -> Just i
           Just r -> if r == rank then Just i else Nothing

scaledIndex :: Int -> Int -> Int -> Int
scaledIndex ranks piles rank = (rank * (piles-1)) `div` ranks

-- Not sure this is right
placeScaledRangeOrStepToTheSide :: Strategy
placeScaledRangeOrStepToTheSide _ piles (Card rank _) =
    let bestIndex = scaledIndex numRanks numPiles rank
    in case Map.lookup bestIndex piles of
           Nothing -> Just bestIndex
           Just r -> case compare r rank of
                         EQ -> Just bestIndex
                         LT -> searchBy bestIndex (<) (+ 1)
                         GT -> searchBy bestIndex (>) (subtract 1)
    where searchBy i cmp next =
              if i < 0 || i >= numPiles
              then Nothing
              else case Map.lookup i piles of
                       Nothing -> Just i
                       Just r -> if r == rank
                                 then Just i
                                 else if cmp r rank
                                      then searchBy (next i) cmp next
                                      else Nothing

placeScaledAvailableRange :: Strategy
placeScaledAvailableRange _ piles (Card rank _) =
    findElementInMap rank piles <|>
    let ((lk,lv),(hk,hv)) = bounds piles rank
        availablePiles = hk-lk-1
        availableRanks = hv-lv-1
    in if availablePiles > 0
       then Just $ lk + 1 + scaledIndex availableRanks availablePiles (rank-lv)
       else Nothing

findElementInMap :: Rank -> Piles -> Maybe Int
findElementInMap rank piles = fmap fst $ find ((==rank).snd) $ Map.toList piles

bounds :: Piles -> Rank -> ((Int, Rank), (Int, Rank))
bounds piles rank =
    let (low,high) = Map.partition (<rank) piles
        lowPair = if Map.null low then (-1,0) else Map.findMax low
        highPair = if Map.null high
                   then (numPiles,numRanks+1)
                   else Map.findMin high
    in (lowPair, highPair)

placeMedianPossible :: Strategy
placeMedianPossible _ piles (Card rank _) =
    findElementInMap rank piles <|>
    let ((lk,lv),(hk,hv)) = bounds piles rank
        highestValid = min (hk-1) (lk+rank-lv)
        lowestValid = max (lk+1) (hk-(hv-rank))
    in if hk-lk <= 1
       then Nothing
       else Just $ (lowestValid + highestValid) `div` 2

placeClosestDiscardRatio :: Strategy
placeClosestDiscardRatio _ piles (Card rank _) =
    findElementInMap rank piles <|>
    let ((lk,lv),(hk,hv)) = bounds piles rank
        highestValid = min (hk-1) (lk+rank-lv)
        lowestValid = max (lk+1) (hk-(hv-rank))
        cardsBelow = rank - lv - 1
        cardsAbove = hv - rank - 1
    in if hk-lk <= 1
       then Nothing
       else Just $ fst $ minimumBy (pileSplitCmp `on` snd) $
                [(index, ((cardsBelow,pilesBelow),(cardsAbove,pilesAbove))) |
                    index <- [lowestValid..highestValid],
                    let pilesBelow = index - lk - 1,
                    let pilesAbove = hk - index - 1]

pileSplitCmp :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Ordering
pileSplitCmp a b = discardDiffCmp a b `mappend` discardRatioDiffCmp a b

discardDiffCmp a b = discardDiff a `compare` discardDiff b
    where discardDiff (low, high) = abs $ discard low - discard high
          discard (cards, piles) = cards - piles

discardRatioDiffCmp a b = discardRatioDiff a `compare` discardRatioDiff b
    where discardRatioDiff (low, high) =
              abs $ discardRatio low - discardRatio high
          discardRatio (cards, piles) = (cards - piles) `idiv` cards
          
idiv :: (Integral a, Fractional c) => a -> a -> c
idiv a b = fromIntegral a / fromIntegral b

---------
-- Debug

instance Show GameState where
    show (GameState deck@(Card rank _:_) discard piles) =
        [rankToChar rank]
        ++ " " ++ show (length discard)
        ++ " " ++ map (\k->fromMaybe '_' (Map.lookup k (fmap rankToChar piles))) [0..numPiles-1]
        where rankToChar r = case r of
                10 -> '0'
                11 -> 'J'
                12 -> 'Q'
                13 -> 'K'
                _ -> head (show r)
        
runTests = runTestTT $ TestList [
    placeMedianPossible [] Map.empty (Card 4 Clubs) ~?= Just 1,
    placeMedianPossible
        [] (Map.insert 1 4 Map.empty) (Card 1 Clubs) ~?= Just 0,
    placeMedianPossible
        [] (Map.insert 0 1
               (Map.insert 1 4 Map.empty)) (Card 5 Diamonds) ~?= Just 2,
    placeMedianPossible [] Map.empty (Card 13 Clubs) ~?= Just 9,
    ---
    placeClosestDiscardRatio []
         (Map.fromList [(9,1),(2,3),(6,9),(7,10),(8,11),(9,12)] )
         (Card 7 Clubs)
         ~?= Just 5
    ]


{-

Coins on a Clock: Part I

To play the game, Coins on a Clock, start with four pennies, four nickles, and
four dimes. You must place the twelve total coins onto a clock using the
following rules. Start on the 12 o'clock hour. Place one coin of your choice on
that spot and then move clockwise around the clock a number of hours equal to
the value of the coin. For example, starting on 12 and placing a nickel (worth
5) would land you on the 5 o'clock hour next. Continue placing coins using
these rules until you reach a space where you have already placed a coin. If
you have any coins left to place, you lose. If you have placed every coin on
the clock, you win.

1a) Come up with a placement of coins that wins the game.
1b) What are all the valid placements of coins starting from 12 o'clock?

-}

import Data.List (nub, delete, sort)
import Control.Monad (guard)
import Data.Maybe (isNothing)
import Control.Arrow (first)

numHours = 12
initCoins = concat $ replicate 4 [1,5,10]

main = mapM_ print solutions
solutions = solve [] initCoins numHours

solve clock [] _ = return clock
solve clock coins hour = do
  guard $ isNothing $ lookup hour clock
  coin <- nub coins
  solve ((hour,coin) : clock) (delete coin coins) (move hour coin)

move hour coin = (hour+coin-1) `mod` numHours + 1

{-

Coins on a Clock Part II:

Given a correct solution, where all coins are placed successfully according to
the rules in part one, the score for a given placement is the sum of all coin
values multiplied by the value of the clock number they were each placed on.
What is the highest score you can achieve in Coins on a Clock?

-}

allScores = nub $ sort $ concatMap scores solutions
scores clock = map (scoreClock . rotateClock clock) [1..numHours]
rotations clock = map (rotateClock clock) [1..numHours]
scoreClock = sum . map (uncurry (*))
rotateClock l offset = map (first (move offset)) l


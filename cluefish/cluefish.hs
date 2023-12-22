import Data.FiniteDomain

data Info = Info Player Guess Result
type Player = Int
data Guess = GuessRank Rank | GuessSuit Suit
data Result = Disprove | Pass | Reveal Card

data Card = Card Rank Suit
data Rank = Jack | Queen | King | Ace deriving (Enum, Bounded)
data Suit = Diamonds | Clubs | Hearts | Spades deriving (Enum, Bounded)

test = runFD $ do
    x <- newVar [Jack .. Ace]


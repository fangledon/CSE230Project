-- This is a conceptual version of the haskell interfaces provided by the backend.

-- INFO: All things shown here is public.

data Suit = Heart | Spade | Diamond | Club 
    deriving Eq

data Rank = {-private-}
    deriving (Eq, Ord, Enum)

rk :: {-1~13 or J Q K A-} -> Rank -- smart constructor for rank
-- Note: rk 2 is the minimal rank and rk A is the maximal

data Card = {-private-}
    deriving (Eq, Ord)

rank :: Card -> Rank -- Returns the rank of the card

suit :: Card -> Suit -- Returns the suit of the card

data Player = {-private-}

hand :: Player -> [Card] -- indicates the player's private cards
money :: Player -> Int -- indicates the total remaining money for the player
wager :: Player -> Int -- indicates the bet placed by the player

isFolded :: Player -> Bool
-- Return True if the player has folded
isAllIn :: Player -> Bool
-- Return True if the player has all-ined

data Phase = Preflop | Flop | Turn | River | Endgame
    deriving Enum

data Game = {-private-}

players :: Game -> [Player] -- Return all players in game
public :: Game -> [Card] -- Return public cards in game
phase :: Game -> Phase -- Return current phase in game

newGame :: Int -> Int -> Int -> Int -> Game
-- Return a new game. The 4 int parameters are (sequentially):
-- Number of players
-- Number of starting money of each player
-- Number of the small blind
-- Random seed

isValid :: Game -> Bool
-- Return `True` if the game is in a valid state.
-- WARNING: Functions below return UNDEFINED result if the game is not valid --------------

isTurn :: Game -> Player -> Bool
-- Return `True` if the player is in their turn.
isDealer :: Game -> Player -> Bool
-- Return `True` if the player has the dealer button.
isNextPhaseReady :: Game -> Bool
-- Return `True` if the game should proceed to the next phase.
doNextPhase :: Game -> Game
-- Return the game proceeded into the next phase. Returns an invalid game if not isNextPhaseReady
minimalAdd :: Game -> Player -> Int
-- Return the minimal amount the player is required to add to make a valid move. Return value is undefined if player not in turn.
-- A player can do Pass only if their minimalAdd is 0
maximalAdd :: Game -> Player -> Int
-- Return the maximal amount the player can add to make a valid move. Return value is undefined if player not in turn.

data Action = Fold | Pass | Add Int -- Adding all money is considered all-in
doAction :: Game -> Player -> Action -> Game
-- Return the game proceeded after player do action. Returns an invalid game if not the player's turn or not a valid action'

-- WARNING: The functions below require the game in Endgame state. Return values are UNDEFINED otherwise. ----------
income :: Game -> Player -> Int
-- Return the income the player receives after the game.
-- Note that a player might have income even if their combination is not the best.

data Combination = RoyalFlush Suit 
                | StraightFlush Suit Rank -- Rank is the highest rank
                | Four Rank Card -- Card is the remaining card
                | House [Card] [Card] -- The three and the two
                | Flush [Card]
                | Straight [Card]
                | Three [Card] [Card] -- The three and remaining
                | Pairs [Card] [Card] Card -- First Pair, Second Pair, Remain
                | Pair [Card] [Card] -- Pair, Remain
                | High [Card]
                
cardsOf :: Combination -> [Card]
-- Return the 5 cards of a combination. Return value is undefined if the combination is invalid

combination :: Game -> Player -> Combination
-- Return the (best) combination the player achieves.
isBest :: Game -> Player -> Bool
-- Return whether the player has the best combination of the round.
-- Note that the best player does not necessarily take all the pot.

module Texas.Backend.Player where

import Texas.Backend.Card
import Texas.Backend.Combination

data Player = P {
    hand :: [Card],
    money :: Int,
    wager :: Int,
    isFolded :: Bool,
    isAllIn :: Bool,
    seat :: Int,
    egComb :: Combination
} deriving (Show,Eq)

-- | Build a new player with starting money and seat number
newPlayer :: Int -> Int -> Player
newPlayer sm st = P [] sm 0 False False st $ badComb

-- | Player places a bet
placeBet :: Player -> Int -> Player
placeBet p@P {money = m, wager = w} b = chkAllIn $ p {money = m-b, wager = w+b}
    where chkAllIn :: Player -> Player
          chkAllIn p1@P {money = 0} = p1 {isAllIn = True} 
          chkAllIn rest            = rest


-- >>> newPlayer 10 0
-- P {hand = [], money = 10, wager = 0, isFolded = False, isAllIn = False, seat = 0, egComb = High []}

-- >>> newPlayer 10 0
-- P {hand = [], money = 10, wager = 0, isFolded = False, isAllIn = False, seat = 0, egComb = High []}
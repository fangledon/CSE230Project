module Texas.Backend.Player where

import Texas.Backend.Card
import Texas.Backend.Combination

data Player = P {
    hand :: [Card],
    money :: Int,
    wager :: Int,
    isFolded :: Bool,
    isAllIn :: Bool,
    egComb :: Combination
} deriving (Show,Eq)

-- | Build a new player with starting money
newPlayer :: Int -> Player
newPlayer sm = P [] sm 0 False False $ High []

-- | Player places a bet
placeBet :: Player -> Int -> Player
placeBet (P h m w f a c) b = chkAllIn $ P h (m-b) (w+b) f a c
    where chkAllIn :: Player -> Player
          chkAllIn (P h 0 w f _ c) = P h 0 w f True c 
          chkAllIn rest            = rest

-- update hand
updHand :: Player -> [Card] -> Player
updHand (P _ m w f a c) nh = P nh m w f a c


-- >>> newPlayer 10
-- P {hand = [], money = 10, wager = 0, isFolded = False, isAllIn = False}

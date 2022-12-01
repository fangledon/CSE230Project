module Texas.Backend.Player where

import Texas.Backend.Card

data Player = P {
    hand :: [Card],
    money :: Int,
    wager :: Int,
    isFolded :: Bool,
    isAllIn :: Bool
} deriving Show

-- | Build a new player with starting money
newPlayer :: Int -> Player
newPlayer sm = P [] sm 0 False False

-- >>> newPlayer 10
-- P {hand = [], money = 10, wager = 0, isFolded = False, isAllIn = False}

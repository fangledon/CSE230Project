{-# LANGUAGE InstanceSigs #-}
module Texas.Bot.CallBot
(
    mkCallBot
) where

import Texas.Bot.Interface
import Texas.Backend.Game
import Texas.Backend.Player
import Texas.Backend.Sample

data CallBot = CB 
    deriving (Show)

instance Bot CallBot where
  doDecision :: CallBot -> Game -> Player -> (Action, CallBot)
  doDecision _ g p  | mini == 0 = (DoPass, CB)
                    | otherwise = (DoAdd mini, CB)
    where   mini = minimalAdd g p


-- | A bot that calls. Takes no arguments.
mkCallBot :: CallBot
mkCallBot = CB

-- >>> doDecision mkCallBot ex2Preflop ex2PreflopDaisy
-- Variable not in scope: ex2Preflop :: Game
-- Variable not in scope: ex2PreflopDaisy :: Player

-- >>> doDecision mkCallBot ex2Preflop ex2PreflopDaisy
-- (Add 2,CB)

module Texas.Bot.Interface where

import Texas.Backend.Player
import Texas.Backend.Game

class Bot a where
    doDecision :: a -> Game -> Player -> (Action, a)

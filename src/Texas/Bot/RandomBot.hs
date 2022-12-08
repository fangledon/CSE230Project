{-# LANGUAGE InstanceSigs #-}
module Texas.Bot.RandomBot 
(
    RandomBot,
    mkRandomBot
)
where

import System.Random
import Texas.Bot.Interface
import Texas.Backend.Game
import Texas.Backend.Player
import Texas.Backend.Sample

newtype RandomBot = RB StdGen
    deriving Show
instance Bot RandomBot where
  doDecision :: RandomBot -> Game -> Player -> (Action, RandomBot)
  doDecision (RB rng) g p   | rdFA <= 4 = (DoFold, RB rng1) -- we have a 1/10 chance of folding
                            | rdFA == 5 = (DoAdd maxi, RB rng1) -- another 1/50 chance of all-in
                            | otherwise = (wrap $ DoAdd adv, rb2) -- otherwise we do the exponential add
    where   mini = minimalAdd g p
            maxi = maximalAdd g p
            (rdFA, rng1) = genWord32R 49 rng
            (adv, rb2) = expo (RB rng1) mini maxi
            wrap (DoAdd 0) = DoPass
            wrap _rest     = _rest

-- We have a 3/4 chance to add the minimal amount
-- and another 1/4 chance to increase a random amount and repeat this
expo :: RandomBot -> Int -> Int -> (Int, RandomBot)
expo (RB rng) lb ub     | lb >= ub = (ub, RB rng)
                        | rdDec /= 0 = (lb, RB rng1)
                        | otherwise = expo (RB rng2) (lb+fromIntegral rdAdd) ub
    where   (rdDec, rng1) = genWord32R 3 rng
            maxAdd = max 1 $ (ub-lb+1) `div` 4
            (rdAdd, rng2) = genWord32R (fromIntegral maxAdd) rng1

-- | A bot that makes random decisions. Take random seed as argument
mkRandomBot :: Int -> RandomBot
mkRandomBot seed = RB $ mkStdGen seed

-- >>> doDecision (mkRandomBot 5555555) ex2Preflop ex2PreflopDaisy
-- (Add 2,RB (StdGen {unStdGen = SMGen 18055030907587191552 224520996576449669}))

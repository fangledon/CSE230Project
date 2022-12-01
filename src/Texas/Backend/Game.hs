module Texas.Backend.Game where

import System.Random.Shuffle (shuffle')
import System.Random (StdGen, mkStdGen)

import Texas.Backend.Card
import Texas.Backend.Player

data Phase = Preflop | Flop | Turn | River | Endgame
    deriving (Enum, Bounded, Show)

data Game = G {
    players :: [Player],
    public :: [Card],
    phase :: Phase,
    isValid :: Bool,
    smallBlind :: Int
}   deriving Show

-- | New game builder. The 4 parameters are
-- Number of players, Starting money, Small blind, Random seed
newGame :: Int -> Int -> Int -> Int -> Game
newGame 0 _ _ _      = G [] [] Endgame False 0
newGame 1 _ _ _      = G [] [] Endgame False 0
newGame npl sm bl rs = G [newPlayer sm | _ <- [1..npl]] deck Preflop True bl
    where deck = shuffle' allCards (length allCards) $ mkStdGen rs

-- >>> newGame 2 2 2 2
-- G {players = [P {hand = [], money = 2, wager = 0, isFolded = False, isAllIn = False},P {hand = [], money = 2, wager = 0, isFolded = False, isAllIn = False}], public = [<♦2>,<♠Q>,<♥2>,<♥9>,<♥Q>,<♣A>,<♠2>,<♣5>,<♥K>,<♦4>,<♠5>,<♠J>,<♣2>,<♦J>,<♦Q>,<♣4>,<♠9>,<♣10>,<♣3>,<♠3>,<♦5>,<♣Q>,<♥A>,<♠10>,<♦7>,<♣9>,<♣K>,<♦K>,<♠7>,<♥5>,<♠A>,<♠6>,<♠K>,<♣6>,<♥8>,<♦9>,<♦10>,<♥10>,<♣8>,<♦A>,<♥6>,<♣J>,<♠8>,<♦3>,<♦8>,<♥4>,<♥3>,<♦6>,<♣7>,<♠4>,<♥J>,<♥7>], phase = Preflop, isValid = True, smallBlind = 2}

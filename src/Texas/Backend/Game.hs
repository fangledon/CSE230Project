module Texas.Backend.Game where

import System.Random.Shuffle (shuffle')
import System.Random

import Texas.Backend.Card
import Texas.Backend.Player
import Texas.Backend.Combination

data Phase = Preflop | Flop | Turn | River | Endgame
    deriving (Enum, Bounded, Show)

data Game = G {
    players :: [Player],
    deck :: [Card],
    phase :: Phase,
    ins :: GameState,
    smallBlind :: Int,
    dealerPos :: Int,
    currentPos :: Int,
    public :: [Card],
    rng :: StdGen
}   deriving Show

data GameState = Invalid | Ongoing | Phaseshift | Stuck
    deriving (Eq,Show)

invalid :: Game
invalid = G [] [] Flop Invalid 0 0 0 [] $ mkStdGen 0

isValid :: Game -> Bool
isValid g = ins g /= Invalid

-- | New game builder. The 4 parameters are
-- Number of players, Starting money, Small blind, Random seed
newGame :: Int -> Int -> Int -> Int -> Game
newGame 0 _ _ _      = invalid
newGame 1 _ _ _      = invalid
newGame npl sm bl rs = dealHand $ placeBlind $ G [newPlayer sm | _ <- [1..npl]] deck Preflop Ongoing bl (dp-1) (dp `mod` npl) [] rng3
    where (rng1, rng2) = split $ mkStdGen rs
          (dp, rng3) = uniformR (1, npl) rng2
          deck = shuffle' allCards (length allCards) rng1

-- >>> newGame 4 100 2 999
-- G {players = [P {hand = [], money = 96, wager = 4, isFolded = False, isAllIn = False, egComb = High []},P {hand = [], money = 100, wager = 0, isFolded = False, isAllIn = False, egComb = High []},P {hand = [], money = 100, wager = 0, isFolded = False, isAllIn = False, egComb = High []},P {hand = [], money = 98, wager = 2, isFolded = False, isAllIn = False, egComb = High []}], public = [<♦5>,<♦A>,<♣Q>,<♠5>,<♦2>,<♠A>,<♠9>,<♥8>,<♦3>,<♣7>,<♠6>,<♦Q>,<♦K>,<♠4>,<♠3>,<♠Q>,<♦J>,<♠2>,<♣6>,<♦8>,<♠K>,<♣8>,<♥10>,<♦6>,<♦7>,<♥Q>,<♣A>,<♣J>,<♦4>,<♣10>,<♥K>,<♥A>,<♣3>,<♣9>,<♥2>,<♥5>,<♣K>,<♠J>,<♥7>,<♥6>,<♥4>,<♦9>,<♠8>,<♥9>,<♣2>,<♠7>,<♥3>,<♠10>,<♣5>,<♣4>,<♥J>,<♦10>], phase = Preflop, ins = Ongoing, smallBlind = 2, dealerPos = 2, currentPos = 1, rng = StdGen {unStdGen = SMGen 13699080363776932830 4790414380670817565}}

-- >>> newGame 4 100 2 999
-- G {players = [P {hand = [<♣Q>,<♠5>], money = 96, wager = 4, isFolded = False, isAllIn = False, egComb = High []},P {hand = [<♦2>,<♠A>], money = 100, wager = 0, isFolded = False, isAllIn = False, egComb = High []},P {hand = [<♠9>,<♥8>], money = 100, wager = 0, isFolded = False, isAllIn = False, egComb = High []},P {hand = [<♦5>,<♦A>], money = 98, wager = 2, isFolded = False, isAllIn = False, egComb = High []}], deck = [<♦3>,<♣7>,<♠6>,<♦Q>,<♦K>,<♠4>,<♠3>,<♠Q>,<♦J>,<♠2>,<♣6>,<♦8>,<♠K>,<♣8>,<♥10>,<♦6>,<♦7>,<♥Q>,<♣A>,<♣J>,<♦4>,<♣10>,<♥K>,<♥A>,<♣3>,<♣9>,<♥2>,<♥5>,<♣K>,<♠J>,<♥7>,<♥6>,<♥4>,<♦9>,<♠8>,<♥9>,<♣2>,<♠7>,<♥3>,<♠10>,<♣5>,<♣4>,<♥J>,<♦10>], phase = Preflop, ins = Ongoing, smallBlind = 2, dealerPos = 2, currentPos = 1, public = [], rng = StdGen {unStdGen = SMGen 13699080363776932830 4790414380670817565}}

placeBlind :: Game -> Game
placeBlind g = actPlaceBet (2*sb) $ actPlaceBet sb g
    where sb = smallBlind g

dealHand :: Game -> Game
dealHand g = dhi (npp g (dealerPos g)) g
    where dhi :: Int -> Game -> Game
          dhi p g | p == dealerPos g = dealcur p g
                  | otherwise        = dhi (npp g p) $ dealcur p g 
                where dealcur :: Int -> Game -> Game
                      dealcur pos (G pl pu p3 p4 p5 p6 p7 p8 p9) = let (x, p:ys) = splitAt pos pl
                                                                       (f2, rest) = splitAt 2 pu in
                                                                        G (x ++ updHand p f2 : ys) rest p3 p4 p5 p6 p7 p8 p9

actPlaceBet :: Int -> Game -> Game
actPlaceBet b g@(G pl p2 p3 p4 p5 p6 cp p8 p9) = let (x,p:ys) = splitAt cp pl in
                                            G (x ++ placeBet p b : ys) p2 p3 p4 p5 p6 (npp g cp) p8 p9

numPlayer :: Game -> Int
numPlayer g = length $ players g 

-- Next player pos
npp :: Game -> Int -> Int
npp g x = (x + 1) `mod` numPlayer g 


-- Return `True` if the player is in their turn.
isTurn :: Game -> Player -> Bool
isTurn g p | ins g /= Ongoing   = False
           | otherwise          = p == (players g !! cp)
        where cp = currentPos g

-- Return `True` if the player has the dealer button.
isDealer :: Game -> Player -> Bool
isDealer g p = p == (players g !! dealerPos g)

-- Return `True` if the game should proceed to the next phase.
isNextPhaseReady :: Game -> Bool
isNextPhaseReady g = ins g == Phaseshift

-- Return the game proceeded into the next phase. Returns an invalid game if not isNextPhaseReady
doNextPhase :: Game -> Game
doNextPhase g = if isNextPhaseReady g then nextphase g else invalid
    where nextphase g = case phase g of
            Preflop -> undefined
            Flop -> undefined
            Turn -> undefined
            River -> undefined
            Endgame -> undefined


-- | Return the minimal amount the player is required to add to make a valid move. Return value is undefined if player not in turn.
-- A player can do Pass only if their minimalAdd is 0
minimalAdd :: Game -> Player -> Int
minimalAdd = undefined

-- Return the maximal amount the player can add to make a valid move. Return value is undefined if player not in turn.
maximalAdd :: Game -> Player -> Int
maximalAdd = undefined

data Action = Fold | Pass | Add Int -- Adding all money is considered all-in

-- Return the game proceeded after player do action. Returns an invalid game if not the player's turn or not a valid action'
doAction :: Game -> Player -> Action -> Game
doAction = undefined


-- | Return the income the player receives after the game.
-- Note that a player might have income even if their combination is not the best.
income :: Game -> Player -> Int
income = undefined

-- Return the (best) combination the player achieves.
combination :: Game -> Player -> Combination
combination _ = egComb


-- | Return whether the player has the best combination of the round.
-- Note that the best player does not necessarily take all the pot.
isBest :: Game -> Player -> Bool
isBest = undefined

-- | Return whether everyone has enough money (>= big blind) to continue into the next round
canContinue :: Game -> Bool
canContinue (G pl _ _ _ sb _ _ _ _) = all (\p -> money p >= 2 * sb) pl

-- canContinue g = ins g == Stuck todo

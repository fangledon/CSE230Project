module Texas.Backend.Game where

import System.Random.Shuffle (shuffle')
import System.Random

import Texas.Backend.Card
import Texas.Backend.Player
import Texas.Backend.Combination
import Data.List (subsequences, sort, sortBy)

data Phase = Preflop | Flop | Turn | River | Endgame
    deriving (Enum, Bounded, Show, Eq)

data Game = G {
    players :: [Player],
    deck :: [Card],
    phase :: Phase,
    ins :: GameState,
    smallBlind :: Int,
    dealerPos :: Int,
    currentPos :: Int,
    lastAdd :: Int,
    public :: [Card],
    incomes :: [Int],
    bestPlayers :: [Int],
    rng :: StdGen
}   deriving (Show, Eq)

data GameState = Invalid | Ongoing | Phaseshift | Stuck | Thru
    deriving (Eq,Show)

invalid :: Game
invalid = G [] [] Flop Invalid 0 0 0 0 [] [] [] $ mkStdGen 0

isValid :: Game -> Bool
isValid g = ins g /= Invalid

-- | New game builder. The 4 parameters are
-- Number of players, Starting money, Small blind, Random seed
newGame :: Int -> Int -> Int -> Int -> Game
newGame 0 _ _ _      = invalid
newGame 1 _ _ _      = invalid
newGame npl sm bl rs = dealHand $ placeBlind $ G [newPlayer sm i | i <- [0 .. npl - 1]] deck Preflop Ongoing bl (dp-1) (dp `mod` npl) ((dp + 1) `mod` npl) [] [] [] rng3
    where (rng1, rng2) = split $ mkStdGen rs
          (dp, rng3) = uniformR (1, npl) rng2
          deck = shuffle' allCards (length allCards) rng1

-- >>> newGame 4 100 2 999
-- G {players = [P {hand = [], money = 96, wager = 4, isFolded = False, isAllIn = False, egComb = High []},P {hand = [], money = 100, wager = 0, isFolded = False, isAllIn = False, egComb = High []},P {hand = [], money = 100, wager = 0, isFolded = False, isAllIn = False, egComb = High []},P {hand = [], money = 98, wager = 2, isFolded = False, isAllIn = False, egComb = High []}], public = [<♦5>,<♦A>,<♣Q>,<♠5>,<♦2>,<♠A>,<♠9>,<♥8>,<♦3>,<♣7>,<♠6>,<♦Q>,<♦K>,<♠4>,<♠3>,<♠Q>,<♦J>,<♠2>,<♣6>,<♦8>,<♠K>,<♣8>,<♥10>,<♦6>,<♦7>,<♥Q>,<♣A>,<♣J>,<♦4>,<♣10>,<♥K>,<♥A>,<♣3>,<♣9>,<♥2>,<♥5>,<♣K>,<♠J>,<♥7>,<♥6>,<♥4>,<♦9>,<♠8>,<♥9>,<♣2>,<♠7>,<♥3>,<♠10>,<♣5>,<♣4>,<♥J>,<♦10>], phase = Preflop, ins = Ongoing, smallBlind = 2, dealerPos = 2, currentPos = 1, rng = StdGen {unStdGen = SMGen 13699080363776932830 4790414380670817565}}

-- >>> newGame 4 100 2 999
-- G {players = [P {hand = [<♣Q>,<♠5>], money = 96, wager = 4, isFolded = False, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♦2>,<♠A>], money = 100, wager = 0, isFolded = False, isAllIn = False, seat = 1, egComb = High []},P {hand = [<♠9>,<♥8>], money = 100, wager = 0, isFolded = False, isAllIn = False, seat = 2, egComb = High []},P {hand = [<♦5>,<♦A>], money = 98, wager = 2, isFolded = False, isAllIn = False, seat = 3, egComb = High []}], deck = [<♦3>,<♣7>,<♠6>,<♦Q>,<♦K>,<♠4>,<♠3>,<♠Q>,<♦J>,<♠2>,<♣6>,<♦8>,<♠K>,<♣8>,<♥10>,<♦6>,<♦7>,<♥Q>,<♣A>,<♣J>,<♦4>,<♣10>,<♥K>,<♥A>,<♣3>,<♣9>,<♥2>,<♥5>,<♣K>,<♠J>,<♥7>,<♥6>,<♥4>,<♦9>,<♠8>,<♥9>,<♣2>,<♠7>,<♥3>,<♠10>,<♣5>,<♣4>,<♥J>,<♦10>], phase = Preflop, ins = Ongoing, smallBlind = 2, dealerPos = 2, currentPos = 1, lastAdd = 0, public = [], incomes = [], bestPlayers = [], rng = StdGen {unStdGen = SMGen 13699080363776932830 4790414380670817565}}

resetGame :: Game -> Game
resetGame g = dealHand $ placeBlind $ resetg $ distributeIncome g
    where   resetg :: Game -> Game
            resetg g@G{players = pl, dealerPos = dp, rng = _rng} = g {players = map resetPlayer pl, deck = shuffle' allCards (length allCards) rng1, rng = rng2, dealerPos = ndp,
                                                                        currentPos = ncp, lastAdd = npp g ncp, public = [], phase = Preflop, ins = Ongoing}
                where (rng1, rng2) = split _rng
                      ndp = npp g dp
                      ncp = npp g ndp
            resetPlayer :: Player -> Player
            resetPlayer p = p {hand = [], wager = 0, isFolded = False, isAllIn = False, egComb = badComb}
            distributeIncome :: Game -> Game
            distributeIncome g@G{players = pl, incomes = inc} = g {players = zipWith (\ p i -> p {money = money p + i}) pl inc, incomes = []}
-- >>> resetGame $ 

placeBlind :: Game -> Game
placeBlind g = actPlaceBet (2*sb) $ actPlaceBet sb g
    where sb = smallBlind g

dealHand :: Game -> Game
dealHand g = dhi (npp g (dealerPos g)) g
    where dhi :: Int -> Game -> Game
          dhi p g | p == dealerPos g = dealcur p g
                  | otherwise        = dhi (npp g p) $ dealcur p g
                where dealcur :: Int -> Game -> Game
                      dealcur pos g@G{players = pl, deck = pu} = let (x, p:ys) = splitAt pos pl
                                                                     (f2, rest) = splitAt 2 pu in
                                                                        g {players = x ++ p {hand = f2} : ys, deck = rest}

actPlaceBet :: Int -> Game -> Game
actPlaceBet b g@G{players = pl, currentPos = cp} = let (x,p:ys) = splitAt cp pl in
                                                    g {players = x ++ placeBet p b : ys,currentPos = npp g cp}

updPlayer :: Game -> Player -> Game
updPlayer g@G{players = pl} p = let (x, _:ys) = splitAt (seat p) pl in
                                    g {players = x ++ p : ys}

numPlayer :: Game -> Int
numPlayer g = length $ players g

-- Next player pos
npp :: Game -> Int -> Int
npp g x = (x + 1) `mod` numPlayer g


-- Return `True` if the player is in their turn.
isTurn :: Game -> Player -> Bool
isTurn g p | ins g /= Ongoing   = False
           | otherwise          = seat p == cp
        where cp = currentPos g

-- Return `True` if the player has the dealer button.
isDealer :: Game -> Player -> Bool
isDealer G{dealerPos = dp} p = seat p == dp

-- Return `True` if the game should proceed to the next phase.
isNextPhaseReady :: Game -> Bool
isNextPhaseReady g = ins g `elem` [Phaseshift, Thru]

-- | Return the game proceeded into the next phase. Returns an invalid game if not isNextPhaseReady
-- Will skip phases directly to endgame in the case when  only 1 player is unfolded or each player is either folded or all-ined
doNextPhase :: Game -> Game
doNextPhase g = if isNextPhaseReady g then nextphase' g else invalid
    where nextphase' :: Game -> Game
          nextphase' g = postDo2 $ if ins g == Thru then thruEndgame g else nextphase g
          thruEndgame :: Game -> Game
          thruEndgame g = let npg = nextphase g in
                            if phase npg == Endgame then npg else thruEndgame npg
          nextphase :: Game -> Game
          nextphase g = case phase g of
            Preflop -> pref2Flop g
            Flop -> (turnRiver g) {phase = Turn}
            Turn -> (turnRiver g) {phase = River}
            River -> finishGame g
            Endgame -> resetGame g

postDo :: Game -> Game
postDo g@G{phase = ph} = case ph of
  Endgame -> if isCanContinue g then g else g {ins = Stuck}
  _       -> if isThruEndgame g then g {ins = Thru} else g


finishGame :: Game -> Game
finishGame g = (assignIncome $ assignRank $ assignBestComb g) {phase = Endgame, ins = Phaseshift}

assignIncome :: Game -> Game
assignIncome g@G{players = pl} = g {incomes = assemble $ aiimpl1 g}
    where   assemble t = map snd $ fill0 0 (length pl) $ collect $ sort t
            collect :: [(Int, Int)] -> [(Int, Int)]
            collect (x@(fx,sx):y@(fy,sy):xs)    | fx == fy = collect $ (fx,sx+sy):xs
                                                | otherwise = x : collect (y:xs)
            collect _other = _other
            fill0 :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
            fill0 cr lmt cur    | cr >= lmt = []
                                | otherwise = case cur of
                                                [] -> (cr, 0):fill0 (cr+1) lmt []
                                                (f0,s0):xs -> if f0 == cr then (f0,s0):fill0 (cr+1) lmt xs else (cr,0):fill0 (cr+1) lmt ((f0,s0):xs)


aiimpl1 :: Game -> [(Int, Int)]
aiimpl1 g@G{players = pl, bestPlayers = bp}   = case vp of
                                [] -> []
                                _  -> distIncome (mp `div` length el) el $ aiimpl1 ng
    where   vp  = filter (\P{wager = w} -> w > 0) pl
            mw  = minimum $ map wager vp
            mp  = length vp * mw
            mr  = minimum $ map (\P{seat = se} -> bp !! se) vp
            el  = filter (\P{seat = se} -> (bp !! se) == mr) vp
            ng  = seqDeductW vp mw g
            seqDeductW :: [Player] -> Int -> Game -> Game
            seqDeductW [] _ g = g
            seqDeductW (p@P{wager = w}:ps) dw g = seqDeductW ps dw $ updPlayer g $ p {wager = w-dw}
            distIncome :: Int -> [Player] -> [(Int, Int)] -> [(Int, Int)]
            distIncome _ [] cur = cur
            distIncome ic (P{seat = se}:ps) cur = (se, ic):distIncome ic ps cur


assignBestComb :: Game -> Game
assignBestComb g@G{players = pl, public = pu} = g{players = map (assignBestCombPlayer pu) pl}
    where assignBestCombPlayer :: [Card] -> Player -> Player
          assignBestCombPlayer _ pl@P{isFolded = True} = pl {egComb = badComb}
          assignBestCombPlayer pub pl = pl {egComb = head sortedCombs}
            where combCards = filter (\l -> length l == 5) $ subsequences (pub ++ hand pl)
                  combs = map cardsToComb combCards
                  sortedCombs = sortBy (flip compare) combs

assignRank :: Game -> Game
assignRank g@G{players = pl} = g {bestPlayers = nbp}
    where sortedPlayerIndex = sortBy (flip comparer) [0..(length pl - 1)]
          comparer :: Int -> Int -> Ordering
          comparer l r = compare (egComb $ pl !! l) (egComb $ pl !! r)
          scanner :: Int -> [Int] -> [(Int, Int)]
          scanner t (x:y:xs) = (x,t):scanner (if comparer x y == EQ then t else t+1) (y:xs)
          scanner t [x]      = [(x,t)]
          scanner _ []       = error "invalid call to assignrank"
          nbp = map snd $ sort $ scanner 0 sortedPlayerIndex



isThruEndgame :: Game -> Bool
isThruEndgame G{players = pl} = length (filter (not . isFolded) pl) == 1 || all (\p -> isFolded p || isAllIn p) pl

pref2Flop :: Game -> Game
pref2Flop g = (resetTurn $ dealPublic $ dealPublic $ dealPublic $ burnCard g) {phase = Flop, ins = Ongoing}

turnRiver :: Game -> Game
turnRiver g = (resetTurn $ dealPublic $ burnCard g) {ins = Ongoing}

resetTurn :: Game -> Game
resetTurn g@G{dealerPos = dp} = g {currentPos = np, lastAdd = np}
    where np = npp g dp

dealPublic :: Game -> Game
dealPublic g@G{deck = (d:ds), public = pu}  = g {deck = ds, public = pu ++ [d]}
dealPublic _                                = invalid

burnCard :: Game -> Game
burnCard g@G{deck = (_:ds)} = g {deck = ds}
burnCard _                  = invalid


-- | Return the minimal amount the player is required to add to make a valid move. Return value is undefined if player not in turn.
-- A player can do Pass only if their minimalAdd is 0
minimalAdd :: Game -> Player -> Int
minimalAdd G{players = pl, lastAdd = la} P{wager = w, money = m} = min m $ wager (pl !! la) - w

-- Return the maximal amount the player can add to make a valid move. Return value is undefined if player not in turn.
maximalAdd :: Game -> Player -> Int
maximalAdd _ = money
-- for now assume one can always all-in


data Action = DoFold | DoPass | DoAdd Int -- Adding all money is considered all-in
    deriving (Eq, Show)

-- Return the game proceeded after player do action. Returns an invalid game if not the player's turn or not a valid action'
doAction :: Game -> Player -> Action -> Game
doAction g p act = if not (isValidAction g p act) then invalid else postDo1 $ daimpl g p act

postDo1 :: Game -> Game
postDo1 g@G{players = pl, currentPos = cp, lastAdd = la, smallBlind = smb, phase = ph, ins = st}
        | st /= Ongoing                                     = postDo g
        | cp == la && ph == Preflop && wager cpl == 2 * smb = postDo g 
        | cp == la                                          = postDo g {ins = Phaseshift}
        | isFolded cpl || isAllIn cpl                       = postDo1 g {currentPos = np}
        | otherwise                                         = postDo g
    where   cpl = pl !! cp
            np  = npp g cp

postDo2 :: Game -> Game
postDo2 g@G{players = pl, ins = st, currentPos = cp}
        | st /= Ongoing = postDo g 
        | isFolded cpl || isAllIn cpl = postDo2 g {currentPos = np}
        | otherwise = postDo g 
    where cpl = pl !! cp 
          np  = npp g cp

daimpl :: Game -> Player -> Action -> Game
daimpl g@G{currentPos = cp, lastAdd = la, phase = ph} p DoFold = if ph == Preflop && cp == la then updated {ins = Phaseshift} else updated {currentPos = np}
    where   updated = updPlayer g p {isFolded = True}
            np      = npp g cp
daimpl g@G{currentPos = cp, lastAdd = la, phase = ph} _ DoPass = if ph == Preflop && cp == la then g {ins = Phaseshift} else g {currentPos = np}
    where   np  = npp g cp
daimpl g p (DoAdd ad)                                     | ad > minimalAdd g p = (actPlaceBet ad g) {lastAdd = seat p}
                                                        | otherwise           = actPlaceBet ad g

isValidAction :: Game -> Player -> Action -> Bool
isValidAction g p (DoAdd 0)  = False
isValidAction g p (DoAdd ad) = isValidPlayerForAction g p && ad >= minimalAdd g p && ad <= maximalAdd g p
isValidAction g p DoPass     = isValidPlayerForAction g p && minimalAdd g p == 0
isValidAction g p DoFold     = isValidPlayerForAction g p

isValidPlayerForAction :: Game -> Player -> Bool
isValidPlayerForAction G{currentPos = cp, ins = st} P{seat = se} = st == Ongoing && se == cp

-- | Return the income the player receives after the game.
-- Note that a player might have income even if their combination is not the best.
income :: Game -> Player -> Int
income G {incomes = inc} P {seat = s} = inc !! s

-- Return the (best) combination the player achieves.
combination :: Game -> Player -> Combination
combination _ = egComb


-- | Return whether the player has the best combination of the round.
-- Note that the best player does not necessarily take all the pot.
isBest :: Game -> Player -> Bool
isBest g p = (bestPlayers g !! seat p) == 0

-- | Return whether everyone has enough money (>= big blind) to continue into the next round
canContinue :: Game -> Bool
canContinue g = ins g /= Stuck

isCanContinue :: Game -> Bool
isCanContinue g@G{players = pl, smallBlind = sb} = all (\p -> money p + income g p >= 2 * sb) pl

-- DISPLAY TYPES ---------------------------------------------------------------
data Name = Fold | Check | Call | Raise Int | AllIn | Next
    deriving (Eq, Show, Ord)

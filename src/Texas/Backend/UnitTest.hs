module Texas.Backend.UnitTest where

import Texas.Backend.Game
import Texas.Backend.Player
import System.Random
import Texas.Backend.Card
import Texas.Backend.Combination

import Texas.Backend.Sample

type TestResult = Either (String, Game) Game

tryGame :: String -> Game -> Game -> TestResult
tryGame msg sg tg = if not $ isValid tg then Left (msg, sg) else Right tg

tryNextPhase :: Game -> TestResult
tryNextPhase g = tryGame "NextPhase" g ng
    where ng = doNextPhase g

tryDoAction :: Game -> Player -> Action -> TestResult
tryDoAction g p a = tryGame ("Action " ++ show a ++ " on Player " ++ show (seat p)) g ng
    where ng = doAction g p a

checkPoint :: String -> Game -> Game -> TestResult
checkPoint msg g cp | g == cp   = Right g
                    | otherwise = Left (msg, g)

ensuref :: String -> Game -> Bool -> TestResult
ensuref _ g True = Right g
ensuref msg g False = Left (msg, g)



unitTestEx1 :: TestResult
unitTestEx1 = do
                ensuref "Minwager bob ex1Preflop" ex1Preflop (minimalAdd ex1Preflop ex1PreflopBob == 1)
                t1 <- tryDoAction ex1Preflop ex1PreflopBob (Add 1)
                ensuref "Minwager carol ex1Preflop" t1 (minimalAdd t1 (carol t1) == 0)
                ensuref "Preflop carol in turn" t1 (isTurn t1 (carol t1))
                t2 <- tryDoAction t1 (carol t1) Pass
                ensuref "Turnshift ex1preflop" t2 (isNextPhaseReady t2)
                flop1 <- tryNextPhase t2
                ensuref "Bob Turn1 ex1Flop" flop1 (isTurn flop1 (bob flop1))
                ensuref "Minwager bob1 ex1Flop" flop1 (minimalAdd flop1 (bob flop1) == 0)
                flop2 <- tryDoAction flop1 (bob flop1) Pass
                ensuref "Carol turn1 ex1flop" flop2 $ isTurn flop2 (carol flop2)
                ensuref "minwager carol1 ex1flop" flop2 $ minimalAdd flop2 (carol flop2) == 0
                flop3 <- tryDoAction flop2 (carol flop2) (Add 2)
                ensuref "Alice turn1 ex1flop" flop3 $ isTurn flop3 (alice flop3)
                ensuref "minwager alice1 ex1flop" flop3 $ minimalAdd flop3 (alice flop3) == 2
                flop4 <- tryDoAction flop3 (alice flop3) $ Add 4
                ensuref "Bob turn2 ex1flop" flop4 $ isTurn flop4 (bob flop4)
                ensuref "minwager bob2 ex1flop" flop4 $ minimalAdd flop4 (bob flop4) == 4
                flop5 <- tryDoAction flop4 (bob flop4) $ Add 4
                ensuref "Carol turn2 ex1flop" flop5 $ isTurn flop5 (carol flop5)
                ensuref "minwager carol2 ex1flop" flop5 $ minimalAdd flop5 (carol flop5) == 2
                checkPoint "ex1Flop" flop5 ex1Flop
                flop6 <- tryDoAction flop5 (carol flop5) $ Add 2
                ensuref "Turnshift ex1flop" flop6 $ isNextPhaseReady flop6
                turn1 <- tryNextPhase flop6
                ensuref "Bob turn ex1turn" turn1 $ isTurn turn1 (bob turn1)
                ensuref "minwager bob ex1turn" turn1 $ minimalAdd turn1 (bob turn1) == 0
                turn2 <- tryDoAction turn1 (bob turn1) Pass
                ensuref "Carol turn ex1turn" turn2 $ isTurn turn2 (carol turn2)
                ensuref "minwager carol ex1turn" turn2 $ minimalAdd turn2 (carol turn2) == 0
                turn3 <- tryDoAction turn2 (carol turn2) Pass
                ensuref "Alice turn ex1turn" turn3 $ isTurn turn3 (alice turn3)
                ensuref "minwager alice ex1turn" turn3 $ minimalAdd turn3 (alice turn3) == 0
                checkPoint "ex1Turn" turn3 ex1Turn
                turn4 <- tryDoAction turn3 (alice turn3) Pass
                ensuref "Turnshift ex1turn" turn4 $ isNextPhaseReady turn4
                river1 <- tryNextPhase turn4
                ensuref "Bob turn ex1river" river1 $ isTurn river1 $ bob river1
                ensuref "minwager bob ex1river" river1 $ minimalAdd river1 (bob river1) == 0
                river2 <- tryDoAction river1 (bob river1) $ Add 4
                ensuref "Carol turn ex1river" river2 $ isTurn river2 $ carol river2
                ensuref "minwager carol ex1river" river2 $ minimalAdd river2 (carol river2) == 4
                river3 <- tryDoAction river2 (carol river2) $ Add 4
                ensuref "Alice turn ex1river" river3 $ isTurn river3 $ alice river3
                ensuref "minwager alice ex1river" river3 $ minimalAdd river3 (alice river3) == 4
                river4 <- tryDoAction river3 (alice river3) Fold
                ensuref "Phaseshift ex1river" river4 $ isNextPhaseReady river4
                checkPoint "ckpt ex1River" river4 ex1River
                endgame <- tryNextPhase river4
                ensuref "Phaseshift ex1endgame" endgame $ isNextPhaseReady endgame
                checkPoint "ckpt ex1Endgame" endgame ex1Endgame
                Left ("Good!", endgame)
    where   alice g = head (players g)
            bob   g = players g !! 1
            carol g = players g !! 2
            ted g   = players g !! 3

-- >>> unitTestEx1
-- Left ("Good!",G {players = [P {hand = [<♣A>,<♣7>], money = 44, wager = 6, isFolded = True, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♠Q>,<♥9>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 1, egComb = Three [<♦9>,<♣9>,<♥9>] [<♣K>,<♠Q>]},P {hand = [<♠K>,<♥J>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 2, egComb = Pairs [<♦9>,<♣9>] [<♣K>,<♠K>] <♥J>},P {hand = [<♥2>,<♠4>], money = 50, wager = 0, isFolded = True, isAllIn = False, seat = 3, egComb = High []}], deck = [<♦3>], phase = Endgame, ins = Phaseshift, smallBlind = 1, dealerPos = 0, currentPos = 1, lastAdd = 1, public = [<♦9>,<♠5>,<♥3>,<♣K>,<♣9>], incomes = [0,26,0,0], bestPlayers = [2,0,1,2], rng = StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}})

-- >>> doNextPhase ex1River
-- G {players = [P {hand = [<♣A>,<♣7>], money = 44, wager = 6, isFolded = True, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♠Q>,<♥9>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 1, egComb = Three [<♣9>,<♦9>,<♥9>] [<♣K>,<♠Q>]},P {hand = [<♠K>,<♥J>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 2, egComb = Pairs [<♣9>,<♦9>] [<♣K>,<♠K>] <♥J>},P {hand = [<♥2>,<♠4>], money = 50, wager = 0, isFolded = True, isAllIn = False, seat = 3, egComb = High []}], deck = [<♦3>], phase = Endgame, ins = Phaseshift, smallBlind = 1, dealerPos = 0, currentPos = 1, lastAdd = 1, public = [<♣9>,<♣K>,<♥3>,<♠5>,<♦9>], incomes = [0,26,0,0], bestPlayers = [2,0,1,2], rng = StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}}

-- >>> doNextPhase $ doNextPhase ex1River
-- G {players = [P {hand = [<♥9>,<♣J>], money = 44, wager = 0, isFolded = False, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♦4>,<♠7>], money = 66, wager = 0, isFolded = False, isAllIn = False, seat = 1, egComb = High []},P {hand = [<♦10>,<♣8>], money = 39, wager = 1, isFolded = False, isAllIn = False, seat = 2, egComb = High []},P {hand = [<♦Q>,<♠Q>], money = 48, wager = 2, isFolded = False, isAllIn = False, seat = 3, egComb = High []}], deck = [<♥K>,<♣A>,<♣Q>,<♦9>,<♥5>,<♦K>,<♦8>,<♣10>,<♠6>,<♠3>,<♠J>,<♣2>,<♦6>,<♥Q>,<♥6>,<♦3>,<♦5>,<♥3>,<♣6>,<♠K>,<♠10>,<♣9>,<♥2>,<♠9>,<♥4>,<♣K>,<♥8>,<♦J>,<♣4>,<♠4>,<♥A>,<♥10>,<♦A>,<♣5>,<♥7>,<♠A>,<♠2>,<♣3>,<♠8>,<♦2>,<♣7>,<♠5>,<♥J>,<♦7>], phase = Preflop, ins = Ongoing, smallBlind = 1, dealerPos = 1, currentPos = 0, lastAdd = 3, public = [], incomes = [], bestPlayers = [2,0,1,2], rng = StdGen {unStdGen = SMGen 16204969531660614133 5610259966137620355}}

-- >>> doAction (doNextPhase $ doNextPhase ex1River) (P {hand = [C Heart R09, C Club R0j], money = 44, wager = 0, isFolded = False, isAllIn = False, seat = 0, egComb = High []}) Fold
-- G {players = [P {hand = [<♥9>,<♣J>], money = 44, wager = 0, isFolded = True, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♦4>,<♠7>], money = 66, wager = 0, isFolded = False, isAllIn = False, seat = 1, egComb = High []},P {hand = [<♦10>,<♣8>], money = 39, wager = 1, isFolded = False, isAllIn = False, seat = 2, egComb = High []},P {hand = [<♦Q>,<♠Q>], money = 48, wager = 2, isFolded = False, isAllIn = False, seat = 3, egComb = High []}], deck = [<♥K>,<♣A>,<♣Q>,<♦9>,<♥5>,<♦K>,<♦8>,<♣10>,<♠6>,<♠3>,<♠J>,<♣2>,<♦6>,<♥Q>,<♥6>,<♦3>,<♦5>,<♥3>,<♣6>,<♠K>,<♠10>,<♣9>,<♥2>,<♠9>,<♥4>,<♣K>,<♥8>,<♦J>,<♣4>,<♠4>,<♥A>,<♥10>,<♦A>,<♣5>,<♥7>,<♠A>,<♠2>,<♣3>,<♠8>,<♦2>,<♣7>,<♠5>,<♥J>,<♦7>], phase = Preflop, ins = Ongoing, smallBlind = 1, dealerPos = 1, currentPos = 1, lastAdd = 3, public = [], incomes = [], bestPlayers = [2,0,1,2], rng = StdGen {unStdGen = SMGen 16204969531660614133 5610259966137620355}}

-- >>> assignIncome ex1Endgame
-- G {players = [P {hand = [<♣A>,<♣7>], money = 44, wager = 6, isFolded = True, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♠Q>,<♥9>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 1, egComb = Three [<♥9>,<♣9>,<♦9>] [<♠Q>,<♣K>]},P {hand = [<♠K>,<♥J>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 2, egComb = Pairs [<♠K>,<♣K>] [<♣9>,<♦9>] <♥J>},P {hand = [<♥2>,<♠4>], money = 50, wager = 0, isFolded = True, isAllIn = False, seat = 3, egComb = High []}], deck = [], phase = Endgame, ins = Phaseshift, smallBlind = 1, dealerPos = 0, currentPos = 1, lastAdd = 0, public = [<♣9>,<♣K>,<♥3>,<♠5>,<♦9>], incomes = [0,26,0,0], bestPlayers = [2,0,1,2], rng = StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}}

-- >>> doNextPhase ex1Endgame
-- G {players = [P {hand = [<♥9>,<♣J>], money = 44, wager = 0, isFolded = False, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♦4>,<♠7>], money = 66, wager = 0, isFolded = False, isAllIn = False, seat = 1, egComb = High []},P {hand = [<♦10>,<♣8>], money = 39, wager = 1, isFolded = False, isAllIn = False, seat = 2, egComb = High []},P {hand = [<♦Q>,<♠Q>], money = 48, wager = 2, isFolded = False, isAllIn = False, seat = 3, egComb = High []}], deck = [<♥K>,<♣A>,<♣Q>,<♦9>,<♥5>,<♦K>,<♦8>,<♣10>,<♠6>,<♠3>,<♠J>,<♣2>,<♦6>,<♥Q>,<♥6>,<♦3>,<♦5>,<♥3>,<♣6>,<♠K>,<♠10>,<♣9>,<♥2>,<♠9>,<♥4>,<♣K>,<♥8>,<♦J>,<♣4>,<♠4>,<♥A>,<♥10>,<♦A>,<♣5>,<♥7>,<♠A>,<♠2>,<♣3>,<♠8>,<♦2>,<♣7>,<♠5>,<♥J>,<♦7>], phase = Preflop, ins = Ongoing, smallBlind = 1, dealerPos = 1, currentPos = 0, lastAdd = 3, public = [], incomes = [], rng = StdGen {unStdGen = SMGen 16204969531660614133 5610259966137620355}}

-- >>> assignRank ex1Endgame
-- G {players = [P {hand = [<♣A>,<♣7>], money = 44, wager = 6, isFolded = True, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♠Q>,<♥9>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 1, egComb = Three [<♥9>,<♣9>,<♦9>] [<♠Q>,<♣K>]},P {hand = [<♠K>,<♥J>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 2, egComb = Pairs [<♠K>,<♣K>] [<♣9>,<♦9>] <♥J>},P {hand = [<♥2>,<♠4>], money = 50, wager = 0, isFolded = True, isAllIn = False, seat = 3, egComb = High []}], deck = [], phase = Endgame, ins = Phaseshift, smallBlind = 1, dealerPos = 0, currentPos = 1, lastAdd = 0, public = [<♣9>,<♣K>,<♥3>,<♠5>,<♦9>], incomes = [0,26,0,0], bestPlayers = [2,0,1,2], rng = StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}}

-- >>> assignBestComb ex1Endgame
-- G {players = [P {hand = [<♣A>,<♣7>], money = 44, wager = 6, isFolded = True, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♠Q>,<♥9>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 1, egComb = Three [<♣9>,<♦9>,<♥9>] [<♣K>,<♠Q>]},P {hand = [<♠K>,<♥J>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 2, egComb = Pairs [<♣9>,<♦9>] [<♣K>,<♠K>] <♥J>},P {hand = [<♥2>,<♠4>], money = 50, wager = 0, isFolded = True, isAllIn = False, seat = 3, egComb = High []}], deck = [], phase = Endgame, ins = Phaseshift, smallBlind = 1, dealerPos = 0, currentPos = 1, lastAdd = 0, public = [<♣9>,<♣K>,<♥3>,<♠5>,<♦9>], incomes = [0,26,0,0], bestPlayers = [], rng = StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}}

-- >>> cardsToComb [C Heart R07, C Spade R06, C Spade R05, C Spade R04, C Spade R03]
-- Straight [<♥7>,<♠6>,<♠5>,<♠4>,<♠3>]

-- >>> cardsToComb [C Spade R09, C Spade R06, C Spade R05, C Spade R04, C Spade R03]
-- Flush [<♠9>,<♠6>,<♠5>,<♠4>,<♠3>]

-- >>> cardsToComb [C Spade R0a, C Spade R0k, C Spade R0j, C Spade R0q, C Spade R10]
-- RoyalFlush ♠

-- >>> cardsToComb [C Spade R0a, C Heart R03, C Diamond R05, C Club R07, C Heart R09]
-- High [<♠A>,<♥3>,<♦5>,<♣7>,<♥9>]

-- >>> cardsToComb [C Spade R0k, C Heart R0q, C Club R0k, C Diamond R0q, C Heart R0k]
-- House [<♠K>,<♣K>,<♥K>] [<♥Q>,<♦Q>]
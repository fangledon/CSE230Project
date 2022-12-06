module Texas.Backend.UnitTest where

import Texas.Backend.Game 
import Texas.Backend.Player
import System.Random
import Texas.Backend.Card
import Texas.Backend.Combination



ex1Endgame = G{ players = [ex1Endgame_alice, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    seat = 1,
    egComb = Three [C Heart R09, C Club R09, C Diamond R09] [C Spade R0q, C Club R0k]
    }, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    seat = 2,
    egComb = Pairs [C Spade R0k, C Club R0k] [C Club R09, C Diamond R09] $ C Heart R0j
    }, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    seat = 3,
    egComb = High []
}], public = [C Club R09, C Club R0k, C Heart R03, C Spade R05, C Diamond R09],
    phase = Endgame,
    ins = Phaseshift,
    smallBlind = 1,
    dealerPos = 0,
    currentPos = 1,
    lastAdd = 0,
    deck = [],
    bestPlayers = [2, 0, 1, 2],
    incomes = [0,26,0,0],
    rng = mkStdGen 1
}

ex1Endgame_alice = P { 
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = True,
    isAllIn = False,
    seat = 0,
    egComb = High []
    }

ex1Endgame_ted = P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    seat = 3,
    egComb = High []
}

ex1River = G { players = [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = True,
    isAllIn = False,
    seat = 0,
    egComb = badComb
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    seat = 1,
    egComb = badComb
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    seat = 2,
    egComb = badComb
}, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    seat = 3,
    egComb = badComb
}], public = [C Club R09, C Club R0k, C Heart R03, C Spade R05, C Diamond R09], phase = River,
smallBlind = 1,
ins = Phaseshift,
dealerPos = 0,
currentPos = 1,
lastAdd = 1,
incomes = [],
bestPlayers  = [],
rng = mkStdGen 1,
deck = []
}

-- >>> doNextPhase ex1River
-- G {players = [P {hand = [<♣A>,<♣7>], money = 44, wager = 6, isFolded = True, isAllIn = False, seat = 0, egComb = High []},P {hand = [<♠Q>,<♥9>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 1, egComb = Three [<♣9>,<♦9>,<♥9>] [<♣K>,<♠Q>]},P {hand = [<♠K>,<♥J>], money = 40, wager = 10, isFolded = False, isAllIn = False, seat = 2, egComb = Pairs [<♣9>,<♦9>] [<♣K>,<♠K>] <♥J>},P {hand = [<♥2>,<♠4>], money = 50, wager = 0, isFolded = True, isAllIn = False, seat = 3, egComb = High []}], deck = [], phase = Endgame, ins = Phaseshift, smallBlind = 1, dealerPos = 0, currentPos = 1, lastAdd = 1, public = [<♣9>,<♣K>,<♥3>,<♠5>,<♦9>], incomes = [0,26,0,0], bestPlayers = [2,0,1,2], rng = StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}}

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
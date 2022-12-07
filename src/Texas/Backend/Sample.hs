module Texas.Backend.Sample 
where
import Texas.Backend.Combination
import Texas.Backend.Card
import Texas.Backend.Game
import Texas.Backend.Player
import System.Random (mkStdGen)

-- Begin combination examples
combRoyalFlush = RoyalFlush Heart
combStraightFlush = StraightFlush Spade R09
combFour = Four R10 $ C Spade R0q 
combHouse = House [C Spade R03, C Heart R03, C Club R03] [C Spade R02, C Diamond R02]
combFlush = Flush [C Heart R07, C Heart R08, C Heart R0j, C Heart R0a, C Heart R10]
combStraight = Straight [C Diamond R0q, C Diamond R10, C Club R0j, C Heart R08, C Heart R09]
combThree = Three [C Heart R08, C Spade R08, C Diamond R08] [C Heart R03, C Diamond R0a]
combPairs = Pairs [C Heart R04, C Spade R04] [C Club R07, C Diamond R07] $ C Diamond R10
combPair = Pair [C Diamond R0j, C Club R0j] [C Heart R03, C Spade R0k, C Heart R05]
combHigh = High [C Heart R0a, C Spade R03, C Club R05, C Diamond R07, C Diamond R09]



-- Begin game examples
-- First set of examples are from wikipedia https://en.wikipedia.org/wiki/Texas_hold_%27em
ex1Preflop = G{ players = [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 48,
    wager = 2,
    isFolded = False,
    isAllIn = False,
    seat = 0,
    egComb = badComb
}, ex1PreflopBob, ex1PreflopCarol, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    seat = 3,
    egComb = badComb
}], public = [], phase = Preflop,currentPos = 1, dealerPos = 0, deck = [C Heart R0a, C Club R09, C Club R0k, C Heart R03, C Spade R0a, C Spade R05, C Diamond R0a, C Diamond R09, C Diamond R03],
incomes = [], bestPlayers = [], rng = mkStdGen 1, ins = Ongoing, smallBlind = 1, lastAdd = 2
}

ex1PreflopBob = P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 49,
    wager = 1,
    isFolded = False,
    isAllIn = False,
    seat = 1,
    egComb = badComb
}

ex1PreflopCarol = P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 48,
    wager = 2,
    isFolded = False,
    isAllIn = False,
    seat = 2,
    egComb  = badComb
}

ex1Flop = G { players = [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    seat = 0,
    egComb = badComb
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    seat = 1,
    egComb = badComb
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 46,
    wager = 4,
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
}], public = [C Heart R03, C Club R0k, C Club R09], phase = Flop, smallBlind = 1, lastAdd = 0,
currentPos = 2, dealerPos = 0, deck = [C Spade R0a, C Spade R05, C Diamond R0a, C Diamond R09, C Diamond R03],
incomes = [], bestPlayers = [], rng = mkStdGen 1, ins = Ongoing
}

ex1Turn = G {players = [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    seat = 0,
    egComb = badComb
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    seat = 1,
    egComb = badComb
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 44,
    wager = 6,
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
}], public = [C Spade R05, C Heart R03, C Club R0k, C Club R09], currentPos = 0, dealerPos = 0, phase = Turn, lastAdd = 1,
 incomes = [], bestPlayers = [], rng = mkStdGen 1, ins = Ongoing, smallBlind = 1, deck = [C Diamond R0a, C Diamond R09, C Diamond R03]
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
}], public = [C Diamond R09, C Spade R05, C Heart R03, C Club R0k, C Club R09], phase = River,
smallBlind = 1,
ins = Phaseshift,
dealerPos = 0,
currentPos = 1,
lastAdd = 1,
incomes = [],
bestPlayers  = [],
rng = mkStdGen 1,
deck = [C Diamond R03]
}

ex1Endgame = G{ players = [ex1Endgame_alice, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    seat = 1,
    egComb = Three [C Diamond R09, C Club R09, C Heart R09] [C Club R0k, C Spade R0q]
    }, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    seat = 2,
    egComb = Pairs [C Diamond R09, C Club R09] [C Club R0k, C Spade R0k] $ C Heart R0j
    }, ex1Endgame_ted],
    public = [C Diamond R09, C Spade R05, C Heart R03, C Club R0k, C Club R09],
    phase = Endgame,
    ins = Phaseshift,
    smallBlind = 1,
    dealerPos = 0,
    currentPos = 1,
    lastAdd = 1,
    deck = [C Diamond R03],
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

-- The second set of examples contains all-in and is more complicated
-- Begin: Alice is the dealer, Betty places small blind $1, Cathy places big blind $2
--      Alice has $1000, Betty has $500, Cathy has $200, Daisy has $20
-- Preflop: Alice deal each player 2 cards
--      Daisy adds $10, Alice calls, Betty calls, Cathy calls
-- Flop: Alice burn a card and deal 3 cards face up
--      Betty adds $90, Cathy calls, Daisy all-in, Alice adds $100, Betty calls, Cathy calls
-- Turn: Alice burn a card and deal a card face up
--      Betty checks, Cathy All-in, Alice calls, Betty all-in, Alice Folds
-- Directly to endgame

ex2EndgameAlice = ex2TurnAlice {money = 800, wager = 200, isFolded = True}
ex2EndgameBetty = ex2TurnBetty {money = 0, wager = 500, isAllIn = True, egComb = Straight [C Spade R08, C Spade R07, C Club R06, C Club R05, C Heart R04]}
ex2EndgameCathy = ex2TurnCathy {money = 0, wager = 200, isAllIn = True, egComb = Straight [C Spade R08, C Spade R07, C Diamond R06, C Diamond R05, C Heart R04]}
ex2EndgameDaisy = ex2TurnDaisy {egComb = House [C Spade R0k, C Heart R0k, C Diamond R0k] [C Heart R04, C Club R04]}

ex2Endgame = ex2Turn {
    players = [ex2EndgameAlice, ex2EndgameBetty, ex2EndgameCathy, ex2EndgameDaisy],
    phase = Endgame,
    ins = Phaseshift,
    currentPos = 1,
    lastAdd = 1,
    public = [C Spade R07, C Spade R08, C Heart R04, C Spade R0k, C Club R04],
    incomes = [0,570,270,80],
    bestPlayers = [2,1,1,0],
    deck = [C Club R0q]
}

ex2FlopAlice = ex2PreflopAlice {money = 990, wager = 10}

ex2FlopBetty = ex2PreflopBetty {money = 400, wager = 100}

ex2FlopCathy = ex2PreflopCathy {money = 100, wager = 100}

ex2FlopDaisy = ex2PreflopDaisy {money = 0, wager = 20, isAllIn = True}

ex2Flop = ex2Preflop {
    players = [ex2FlopAlice, ex2FlopBetty, ex2FlopCathy, ex2FlopDaisy],
    currentPos = 0,
    lastAdd = 1,
    public = [C Heart R04, C Spade R0k, C Club R04],
    deck =  [C Club R0a, C Spade R08, C Club R0k, C Spade R07, C Club R0q],
    phase = Flop
}

ex2TurnAlice = ex2FlopAlice {money = 800, wager = 200}
ex2TurnBetty = ex2FlopBetty {money = 390, wager = 110}
ex2TurnCathy = ex2FlopCathy {money = 0, wager = 200, isAllIn = True}
ex2TurnDaisy = ex2FlopDaisy

ex2Turn = ex2Flop {
    players = [ex2TurnAlice, ex2TurnBetty, ex2TurnCathy, ex2TurnDaisy],
    currentPos = 1,
    lastAdd = 2,
    public = [C Spade R08, C Heart R04, C Spade R0k, C Club R04],
    deck =  [C Club R0k, C Spade R07, C Club R0q],
    phase = Turn
}

ex2PreflopAlice = P {
    hand = [C Spade R0a, C Spade R09],
    money = 1000,
    wager = 0,
    isFolded = False,
    isAllIn = False,
    seat = 0,
    egComb = High []
}

ex2PreflopBetty = P {
    hand = [C Club R05, C Club R06],
    money = 499,
    wager = 1,
    isFolded = False,
    isAllIn = False,
    seat = 1,
    egComb = High []
}

ex2PreflopCathy = P {
    hand = [C Diamond R05, C Diamond R06],
    money = 198,
    wager = 2,
    isFolded = False,
    isAllIn = False,
    seat = 2,
    egComb = High []
}

ex2PreflopDaisy = P {
    hand = [C Heart R0k, C Diamond R0k],
    money = 20,
    wager = 0,
    isFolded = False,
    isAllIn = False,
    seat = 3,
    egComb = High []
}

ex2Preflop = G {
    players = [ex2PreflopAlice, ex2PreflopBetty, ex2PreflopCathy, ex2PreflopDaisy],
    public = [],
    deck = [C Diamond R04, C Club R04, C Spade R0k, C Heart R04, C Club R0a, C Spade R08, C Club R0k, C Spade R07, C Club R0q],
    phase = Preflop,
    currentPos = 3,
    lastAdd = 2,
    dealerPos = 0,
    smallBlind = 1,
    ins = Ongoing,
    bestPlayers = [],
    incomes = [],
    rng = mkStdGen 2
}

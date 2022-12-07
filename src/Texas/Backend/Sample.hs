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
-- Will figure it out later
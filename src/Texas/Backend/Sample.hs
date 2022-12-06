module Texas.Backend.Sample 
(
    Suit,
    Phase,
    rk,
    suit,
    rank,
    cardsOf,
    combRoyalFlush,
    combStraightFlush,
    combFour,
    combHouse,
    combFlush,
    combStraight,
    combThree,
    combPairs,
    combPair,
    combHigh,
    hand,
    money,
    wager,
    isFolded,
    isAllIn,
    players,
    public,
    phase,
    isValid,
    isTurn,
    isDealer,
    isNextPhaseReady,
    minimalAdd,
    maximalAdd,
    income,
    isBest,
    combination,
    canContinue,
    ex1Preflop,
    ex1Flop,
    ex1Turn,
    ex1River,
    ex1Endgame
)
where
import Texas.Backend.Combination
import Texas.Backend.Card
import Texas.Backend.Game (Phase (..))

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

-- Use dummy definitions from here. 
-- Will replace with real ones when finish implementation.
data Player = P {
    hand :: [Card],
    money :: Int,
    wager :: Int,
    isFolded :: Bool,
    isAllIn :: Bool,
    _isturn :: Bool,
    _isdealer :: Bool,
    _minAdd :: Int,
    _maxAdd :: Int,
    _income :: Int,
    _comb :: Combination,
    _isbest :: Bool
}
data Game = G {
    players :: [Player],
    public :: [Card],
    phase :: Phase,
    isNextPhaseReady :: Bool,
    canContinue :: Bool
}

isValid :: Game -> Bool
isValid _ = True
isTurn :: Game -> Player -> Bool
isTurn _ = _isturn
isDealer :: Game -> Player -> Bool
isDealer _ = _isdealer
minimalAdd :: Game -> Player -> Int
minimalAdd _ = _minAdd
maximalAdd :: Game -> Player -> Int
maximalAdd _ = _maxAdd
income :: Game -> Player -> Int
income _ = _income
combination :: Game -> Player -> Combination
combination _ = _comb
isBest :: Game -> Player -> Bool
isBest _ = _isbest


-- Begin game examples
-- First set of examples are from wikipedia https://en.wikipedia.org/wiki/Texas_hold_%27em
ex1Preflop = G [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 48,
    wager = 2,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = True,
    _minAdd = 0,
    _maxAdd = 0,
    _income  = 0,
    _comb = High [],
    _isbest = False
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 49,
    wager = 1,
    isFolded = False,
    isAllIn = False,
    _isturn = True,
    _isdealer = False,
    _minAdd = 1,
    _maxAdd = 49,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 48,
    wager = 2,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _income = 0,
    _maxAdd = 0,
    _comb = High [],
    _isbest = False
}] [] Preflop False False

ex1Flop = G [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = True,
    _minAdd = 0,
    _maxAdd = 0,
    _income  = 0,
    _comb = High [],
    _isbest = False
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 46,
    wager = 4,
    isFolded = False,
    isAllIn = False,
    _isturn = True,
    _isdealer = False,
    _minAdd = 2,
    _maxAdd = 46,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _income = 0,
    _maxAdd = 0,
    _comb = High [],
    _isbest = False
}] [C Club R09, C Club R0k, C Heart R03] Flop False False

ex1Turn = G [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    _isturn = True,
    _isdealer = True,
    _minAdd = 0,
    _maxAdd = 44,
    _income  = 0,
    _comb = High [],
    _isbest = False
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 44,
    wager = 6,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _income = 0,
    _maxAdd = 0,
    _comb = High [],
    _isbest = False
}] [C Club R09, C Club R0k, C Heart R03, C Spade R05] Turn False False

ex1River = G [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = True,
    isAllIn = False,
    _isturn = False,
    _isdealer = True,
    _minAdd = 0,
    _maxAdd = 0,
    _income  = 0,
    _comb = High [],
    _isbest = False
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 0,
    _comb = High [],
    _isbest = False
}, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _income = 0,
    _maxAdd = 0,
    _comb = High [],
    _isbest = False
}] [C Club R09, C Club R0k, C Heart R03, C Spade R05, C Diamond R09] River True False

ex1Endgame = G [P { -- Alice
    hand = [C Club R0a, C Club R07],
    money = 44,
    wager = 6,
    isFolded = True,
    isAllIn = False,
    _isturn = False,
    _isdealer = True,
    _minAdd = 0,
    _maxAdd = 0,
    _income  = 0,
    _comb = High [],
    _isbest = False
}, P{ -- Bob
    hand = [C Spade R0q, C Heart R09],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 26,
    _comb = Three [C Heart R09, C Club R09, C Diamond R09] [C Spade R0q, C Club R0k],
    _isbest = True
}, P { -- Carol
    hand = [C Spade R0k, C Heart R0j],
    money = 40,
    wager = 10,
    isFolded = False,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _maxAdd = 0,
    _income = 0,
    _comb = Pairs [C Spade R0k, C Club R0k] [C Club R09, C Diamond R09] $ C Heart R0j,
    _isbest = False
}, P { -- Ted
    hand = [C Heart R02, C Spade R04],
    money = 50,
    wager = 0,
    isFolded = True,
    isAllIn = False,
    _isturn = False,
    _isdealer = False,
    _minAdd = 0,
    _income = 0,
    _maxAdd = 0,
    _comb = High [],
    _isbest = False
}] [C Club R09, C Club R0k, C Heart R03, C Spade R05, C Diamond R09] Endgame True True

-- The second set of examples contains all-in and is more complicated
-- Will figure it out later
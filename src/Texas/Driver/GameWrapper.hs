

module Texas.Driver.GameWrapper where
import Texas.Backend.Game

-- Game wrapper
data GameWrapper = GW {
    game :: Game,
    tookAction :: Bool,
    currAction :: Action,
    currAdd :: Int
}

-- | New game builder. The 4 parameters are
-- Number of players, Starting money, Small blind, Random seed
newGameState :: Int -> Int -> Int -> Int -> GameWrapper
newGameState npl sm bl rs = GW (newGame npl sm bl rs) False Pass 0

tryAction :: GameWrapper -> Action -> GameWrapper
tryAction gw act = case act of
    Pass    -> gw {tookAction = True, currAction = Pass, currAdd = 0}
    Fold    -> gw {tookAction = True, currAction = Fold, currAdd = 0}
    Add n   -> gw {tookAction = True, currAction = Add (x + n), currAdd = x + n}
        where x = currAdd gw

doNext :: GameWrapper -> GameWrapper
doNext gw@GW{tookAction = False} = gw
doNext gw = if isValid nextGame then 
                (if isNextPhaseReady nextGame then
                    GW (doNextPhase nextGame) False Pass 0
                else
                    GW nextGame False Pass 0)
            else gw
    where   nextGame = doAction g p a
            g = game gw
            p = players g !! currentPos g
            a = currAction gw
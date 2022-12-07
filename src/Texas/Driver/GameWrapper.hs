

module Texas.Driver.GameWrapper where
import Texas.Backend.Game
import Texas.Backend.Player

-- Game wrapper
data GameWrapper = GW {
    game :: Game,
    tookAction :: Bool,
    currAction :: Action,
    currAdd :: Int,
    seed :: Int
}

-- | New game builder. The 4 parameters are
-- Number of players, Starting money, Small blind, Random seed
newGameState :: Int -> Int -> Int -> Int -> GameWrapper
newGameState npl sm bl rs = GW (newGame npl sm bl rs) False DoPass 0 rs

tryAction :: GameWrapper -> Name -> GameWrapper
tryAction gw name = if phase (game gw) == Endgame then gw else 
    case name of
        Fold    -> gw {tookAction = True, currAction = DoFold, currAdd = 0}
        Check   -> gw {tookAction = True, currAction = DoPass, currAdd = 0}
        Call    -> gw {tookAction = True, currAction = DoAdd callMoney, currAdd = callMoney}
        Raise n -> gw {tookAction = True, currAction = DoAdd $ raiseMoney n, currAdd = raiseMoney n}
        AllIn   -> gw {tookAction = True, currAction = DoAdd allInMoney, currAdd = allInMoney}
        _       -> gw
        where   g = game gw
                p = players g !! currentPos g
                callMoney = minimalAdd g p
                raiseMoney n = min allInMoney (n + max (currAdd gw) callMoney)
                allInMoney = money p

doNext :: GameWrapper -> GameWrapper
doNext gw@GW{tookAction = False} =  if phase (game gw) == Endgame then 
                                        GW (doNextPhase $ game gw) False DoPass 0 (seed gw)
                                    else gw
doNext gw = if isValid nextGame then
                (
                if isNextPhaseReady nextGame then
                    GW (doNextPhase nextGame) False DoPass 0 (seed gw)
                else GW nextGame False DoPass 0 (seed gw)
                )
            else gw
    where   nextGame = doAction g p a
            g = game gw
            p = players g !! currentPos g
            a = currAction gw

doReset :: GameWrapper -> GameWrapper
doReset gw = GW (newGame (numPlayer $ game gw) 100 1 (seed gw + 1)) False DoPass 0 (seed gw + 1)

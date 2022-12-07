

module Texas.Driver.GameWrapper where
import Texas.Backend.Game
import Texas.Backend.Player
import System.Random

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
newGameState npl sm bl rs = GW (newGame npl sm bl rs) False Pass 0 rs

tryAction :: GameWrapper -> Action -> GameWrapper
tryAction gw act = case act of
    Pass    -> gw {tookAction = True, currAction = Pass, currAdd = 0}
    Fold    -> gw {tookAction = True, currAction = Fold, currAdd = 0}
    Add n   -> gw {tookAction = True, currAction = Add addAmount, currAdd = addAmount}
        where   prevAdd = currAdd gw
                maxAdd = money (players (game gw) !! currentPos (game gw))
                addAmount = min maxAdd (prevAdd + n)

doNext :: GameWrapper -> GameWrapper
doNext gw@GW{tookAction = False} =  if phase g == Endgame then 
                                        GW (doNextPhase g) False Pass 0 (seed gw)
                                    else gw
                                        where g = game gw
doNext gw = if isValid nextGame then
                (
                if isNextPhaseReady nextGame then
                    GW (doNextPhase nextGame) False Pass 0 (seed gw)
                else GW nextGame False Pass 0 (seed gw)
                )
            else gw
    where   nextGame = doAction g p a
            g = game gw
            p = players g !! currentPos g
            a = currAction gw

doReset :: GameWrapper -> GameWrapper
doReset gw = GW (newGame (numPlayer $ game gw) 100 1 (seed gw + 1)) False Pass 0 (seed gw + 1)



module Texas.Driver.GameWrapper where
import Texas.Backend.Game
import Texas.Backend.Player
import Texas.Bot.RandomBot
import Texas.Bot.Interface (Bot(doDecision))

-- Game wrapper
data GameWrapper = GW {
    game :: Game,
    tookAction :: Bool,
    currAction :: Action,
    currName :: Name,
    currAdd :: Int,
    seed :: Int,
    bots :: [RandomBot]
}

-- | New game builder. The 4 parameters are
-- Number of players, Starting money, Small blind, Random seed
newGameState :: Int -> Int -> Int -> Int -> GameWrapper
newGameState npl sm bl rs = GW (newGame npl sm bl rs) False DoPass Check 0 rs [mkRandomBot (rs+i) | i <- [1..npl]]

tryAction :: GameWrapper -> Name -> GameWrapper
tryAction gw name = if phase (game gw) == Endgame then gw else
    case name of
        Fold    -> gw {tookAction = True, currAction = DoFold, currName = Fold, currAdd = 0}
        Check   -> gw {tookAction = True, currAction = DoPass, currName = Check, currAdd = 0}
        Call    -> gw {tookAction = True, currAction = DoAdd callMoney, currName = Call, currAdd = callMoney}
        Raise n -> gw {tookAction = True, currAction = DoAdd $ raiseMoney n, currName = Raise n, currAdd = raiseMoney n}
        AllIn   -> gw {tookAction = True, currAction = DoAdd allInMoney, currName = AllIn, currAdd = allInMoney}
        _       -> gw
        where   g = game gw
                p = players g !! currentPos g
                callMoney = minimalAdd g p
                raiseMoney n = min allInMoney (n + max (currAdd gw) callMoney)
                allInMoney = money p

doNext :: GameWrapper -> GameWrapper
doNext gw@GW{tookAction = False} =  if (phase (game gw) == Endgame) && canContinue (game gw) then
                                        GW (doNextPhase $ game gw) False DoPass Check 0 (seed gw) $ bots gw
                                    else gw
doNext gw = if isValid nextGame then
                (
                if isNextPhaseReady nextGame then
                    GW (doNextPhase nextGame) False DoPass Check 0 (seed gw) $ bots gw
                else GW nextGame False DoPass Check 0 (seed gw) $ bots gw
                )
            else gw
    where   nextGame = doAction g p a
            g = game gw
            p = players g !! currentPos g
            a = currAction gw

nextTurn :: GameWrapper -> GameWrapper
nextTurn gw@GW{game = g@G{phase = ph, currentPos = cp}}
    | ph == Endgame = gw
    | isNextPhaseReady g = gw
    | cp == 0       = gw
    | otherwise     = botTurn gw

botTurn :: GameWrapper -> GameWrapper
botTurn gw@GW{game = g@G{players = pl, currentPos = cp}, bots = bts} = nextTurn $ doNext gw {tookAction = True, currAction = decision, bots = nbots}
    where cpl = pl !! cp
          cbt = bts !! cp
          (decision, nbt) = doDecision cbt g cpl
          nbots = let (x,_:ys) = splitAt cp bts in x ++ (nbt:ys)

doReset :: GameWrapper -> GameWrapper
doReset gw = GW (newGame (numPlayer $ game gw) 100 1 (seed gw + 1)) False DoPass Check 0 (seed gw + 1) $ bots gw

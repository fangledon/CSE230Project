module Texas.Driver.Render
( 
    drawUI
)
where

import Brick.AttrMap              (attrName)
import Brick.Types                (Widget, Padding(Pad))
import Brick.Widgets.Border       (border, borderWithLabel, hBorder)
import Brick.Widgets.Border.Style 
import Brick.Widgets.Center       (center, hCenter)
import Brick.Widgets.Core

import Texas.Driver.GameWrapper
import Texas.Backend.Game
import Texas.Backend.Card
import Texas.Backend.Player
import Texas.Backend.Combination

-------------------------------------------------------------------------------

cardStyle :: Widget Name -> Widget Name -- borderwithstyle wrapper
cardStyle = withBorderStyle unicodeRounded . border 

emptyCard :: Widget Name -- renders an empty card with no content
emptyCard = withBorderStyle rounded $ border $ str "  "
  where rounded = BorderStyle 
          { bsIntersectFull = toEnum 0x253C
          , bsCornerTL      = toEnum 0x256D , bsCornerTR      = toEnum 0x256E
          , bsCornerBR      = toEnum 0x256F , bsCornerBL      = toEnum 0x2570
          , bsIntersectL    = toEnum 0x251C , bsIntersectR    = toEnum 0x2524
          , bsIntersectT    = toEnum 0x252C , bsIntersectB    = toEnum 0x2534
          , bsHorizontal    = ' '           , bsVertical      = ' '
          }

emptyCards :: Int -> [Widget Name]
emptyCards n = replicate n emptyCard

displayedCard :: Card -> Widget Name               -- renders card internals
displayedCard (C s r) = withAttr (attrName color) -- or a card front.
                      $ cardStyle $ str $ show s ++ rkToStr r
                      where color = if s == Heart || s == Diamond then "redCard" else "blackCard"

drawPublicCards :: [Card] -> Widget Name
drawPublicCards cards = hBox (publicWidgets ++ emptyCards emptyCardsLen)
  where publicWidgets = map displayedCard cards
        emptyCardsLen = 5 - length cards

drawCards :: [Card] -> Widget Name
drawCards cards = hBox (map displayedCard cards)

drawField :: GameWrapper -> Widget Name
drawField gw =  hCenter (drawPublicCards (public g)) <=> 
                hBorder <=> 
                hCenter (drawCards (hand currPlayer)) <=> 
                hCenter (withAttr (attrName "bold") (str playerText)) <=> 
                hCenter (str currentBet) <=>
                hCenter (str playerMoney)
  where g = game gw
        currPlayer = players g !! currentPos g
        playerText = "Player " ++ show (currentPos g)
        currentBet = "Added Bet: " ++ show (currAdd gw)
        playerMoney = "Money: " ++ show (money currPlayer)

drawWinField :: GameWrapper -> Widget Name
drawWinField gw = hCenter (drawPublicCards (public g)) <=> 
                  hBorder <=> 
                  bestPlayersHand g
  where g = game gw

bestPlayersHand :: Game -> Widget Name
bestPlayersHand g =  hCenter $ vBox $ map (playerBestHand g) (players g)

isOnlyUnfolded :: Game -> Player -> Bool
isOnlyUnfolded g p = not (isFolded p) && countFolded g == n - 1
  where countFolded g = length $ filter isFolded (players g)
        n = numPlayer g

playerBestHand:: Game -> Player -> Widget Name
playerBestHand g p =  if isBest g p then
                        hCenter (withAttr (attrName "bold") (str winText)) <=> 
                        if isOnlyUnfolded g p then 
                          emptyWidget
                        else 
                          hCenter (drawCards playerHand) <=> 
                          hCenter (str "Best Combination") <=> 
                          hCenter (drawCards bestComb)
                      else
                        emptyWidget
                      where winText = "Player " ++ show (seat p) ++ " Wins!"
                            bestComb = cardsOf (egComb p)
                            playerHand = hand p

mkNameButton :: Name -> Widget Name
mkNameButton name = reportExtent name (padBottom (Pad 1) $ withAttr (attrName "btnStyle") (str (show name)))

textBox :: String -> Widget Name
textBox s = padBottom (Pad 1)
            $ str s

playerBetBox :: Game -> Int -> Widget Name
playerBetBox g i = padBottom (Pad 1)
                $ withAttr (attrName color) (str $ "P" ++ show i ++ ": " ++ show (wager p))
                where p = players g !! i
                      color = if isDealer g p then "green" else "white"

playerIncomeBox :: Game -> Int -> Widget Name
playerIncomeBox g i = padBottom (Pad 1)
                $ withAttr (attrName color) (str $ "P" ++ show i ++ ": " ++ show income)
                where p = players g !! i
                      income = incomes g !! i
                      color = if isBest g p then "red" else "white"

betsBar :: Game -> Widget Name
betsBar g = textBox "Total Bets" <=>
          vBox (map (playerBetBox g) [0..(numPlayer g - 1)]) <=>
          withAttr (attrName "green") (str "Dealer")

incomesBar :: Game -> Widget Name
incomesBar g = textBox "Incomes" <=>
              vBox (map (playerIncomeBox g) [0..(numPlayer g - 1)])

drawUI :: GameWrapper -> [Widget Name]
drawUI gw = [ui]
  where 
    ui         = center $ setAvailableSize (50,100) $ lSidebar <+> board <+> rSidebar
    title      = " Texas Hold'em "
    g          = game gw
    board      = withBorderStyle unicodeRounded $ borderWithLabel (str title) 
                $ if phase g == Endgame then 
                    drawWinField gw
                  else 
                    drawField gw
    lSidebar   = padAll 1
                $ if phase g == Endgame then 
                    incomesBar g
                  else 
                    betsBar g
    rSidebar   = padAll 1
                $ textBox "Action" <=>
                  vBox (map mkNameButton [Fold, Check, Call, Raise 1, AllIn, Next])

module Texas.Driver.Render
( 
    drawUI
)
where

import Brick.AttrMap              (attrName, getDefaultAttr)
import Brick.Types                (Widget, Padding(Pad))
import Brick.Widgets.Border       (vBorder, border, borderWithLabel, hBorder)
import Brick.Widgets.Border.Style 
import Brick.Widgets.Center       (center, hCenter)
import Brick.Widgets.Core
import Text.Printf                (printf)

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
displayedCard (C s r) = withAttr (attrName "card") -- or a card front.
                      $ cardStyle $ str $ show s ++ rkToStr r

drawPublicCards :: [Card] -> Widget Name
drawPublicCards cards = hBox (publicWidgets ++ emptyCards emptyCardsLen)
  where publicWidgets = map displayedCard cards
        emptyCardsLen = 5 - length cards

drawPrivateCards :: [Card] -> Widget Name
drawPrivateCards cards = hBox (map displayedCard cards)

drawField :: GameWrapper -> Widget Name
drawField gw = hCenter (drawPublicCards (public g)) <=> 
              hBorder <=> 
              hCenter (drawPrivateCards (hand currPlayer)) <=> 
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
                  hCenter (bestPlayersHand g)
  where g = game gw

bestPlayersHand :: Game -> Widget Name
bestPlayersHand g = vBox (map (playerBestHand g) (players g))  

playerBestHand:: Game -> Player -> Widget Name
playerBestHand g p =  if isBest g p then
                    str playerText <=> 
                    drawPublicCards cards
                  else
                    emptyWidget
                  where playerText = "Player " ++ show (seat p) ++ " Wins!"
                        cards = cardsOf (egComb p)

mkActionButton :: Action -> Widget Name
mkActionButton action = reportExtent (Act action)
                        (padBottom (Pad 1) $ withAttr (attrName "underline") (str (show action)))

mkNameButton :: Name -> Widget Name
mkNameButton name = reportExtent name (padBottom (Pad 1) $ withAttr (attrName "underline") (str (show name)))

textNumBox :: String -> Int -> Widget Name
textNumBox s n = padBottom (Pad 1)
                $ str s <+> withAttr (attrName "bold") (str $ show n)

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
              vBox (map (playerIncomeBox g) [0..(numPlayer g - 1)]) <=>
              withAttr (attrName "red") (str "Best Hand")

drawUI :: GameWrapper -> [Widget Name]
drawUI gw = [ui]
  where 
    ui         = center $ setAvailableSize (50,80) $ lSidebar <+> board <+> rSidebar
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
                  vBox (map mkActionButton [Fold, Pass, Add 1]) <=>
                  mkNameButton AllIn <=>
                  mkNameButton Next

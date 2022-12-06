module Texas.Driver.Render
( 
    drawUI
)
where

import Brick.AttrMap              (attrName)
import Brick.Types                (Widget, Padding(Pad)) -- TODO: Padding(Pad)
import Brick.Widgets.Border       (vBorder, border, borderWithLabel, hBorder)
import Brick.Widgets.Border.Style 
import Brick.Widgets.Center       (center)
import Brick.Widgets.Core
import Text.Printf                (printf)

import Texas.Backend.Game
import Texas.Backend.Card
import Texas.Backend.Player

-------------------------------------------------------------------------------

cardStyle :: Widget Name -> Widget Name -- borderwithstyle wrapper
cardStyle = withBorderStyle unicodeRounded . border 

emptyCard :: Card -> Widget Name -- renders an empty card with no content
emptyCard _ = withBorderStyle rounded $ border $ str "  "
  where rounded = BorderStyle 
          { bsIntersectFull = toEnum 0x253C
          , bsCornerTL      = toEnum 0x256D , bsCornerTR      = toEnum 0x256E
          , bsCornerBR      = toEnum 0x256F , bsCornerBL      = toEnum 0x2570
          , bsIntersectL    = toEnum 0x251C , bsIntersectR    = toEnum 0x2524
          , bsIntersectT    = toEnum 0x252C , bsIntersectB    = toEnum 0x2534
          , bsHorizontal    = ' '           , bsVertical      = ' '
          }

displayedCard :: Card -> Widget Name               -- renders card internals
displayedCard (C s r) = withAttr (attrName "card") -- or a card front.
                      $ cardStyle $ str $ show s ++ rkToStr r

drawCards :: [Card] -> Widget Name
drawCards cards = hBox (map displayedCard cards)

drawField :: Game -> Widget Name
drawField g = drawCards (public g) <=> 
              hBorder <=> 
              drawCards (hand currPlayer) <=> 
              textBox playerText <=> 
              textNumBox "Money: " playerMoney <=>
              textNumBox "Wager: " playerWager
  where currPlayer = players g !! currentPos g
        playerText = "Player " ++ show (currentPos g) ++ " Private Cards"
        playerMoney = money currPlayer
        playerWager = wager currPlayer

mkActionButton :: Action -> Widget Name
mkActionButton action = reportExtent (Act action)
                        (padBottom (Pad 1) $ withAttr (attrName "underline") (str (show action)))

mkNextButton :: String -> Widget Name
mkNextButton s = reportExtent Next (padBottom (Pad 1) $ withAttr (attrName "underline") (str s))

textNumBox :: String -> Int -> Widget Name
textNumBox s n = padBottom (Pad 1)
                $ str s <+> withAttr (attrName "bold") (str $ printf "%3d" n)

textBox :: String -> Widget Name
textBox s = padBottom (Pad 1)
            $ str s

playerBetBox :: Player -> Widget Name
playerBetBox p = padBottom (Pad 1)
                $ str ("P" ++ show (seat p) ++ ": " ++ show (wager p)) 

drawUI :: Game -> [Widget Name]
drawUI game = [ui]
  where 
    ui         = center $ setAvailableSize (120,29) $ lSidebar <+> board <+> rSidebar
    title      = " Texas Hold'em "
    board      = withBorderStyle unicodeRounded
               $ borderWithLabel (str title) 
               $ drawField game
    lSidebar   = padAll 1
                $ textBox "Placed Bet" <=>
                  vBox (map playerBetBox (players game))
    rSidebar   = padAll 1
                $ textBox "Action" <=>
                  vBox (map mkActionButton [Fold, Pass, Add 2]) <=>
                  mkNextButton "Next"

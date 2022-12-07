module Texas.Driver.Entry 
(
    client
)
where

import Control.Monad                (void)
import Graphics.Vty as Vty
import Lens.Micro.TH                (makeLenses)
import qualified System.Random as R (next, newStdGen)

import Brick.AttrMap 
import Brick.Main
import Brick.Types 
import Brick.Util 

import Texas.Driver.Render    (drawUI)
import Texas.Driver.GameWrapper
import Texas.Backend.Game
import Texas.Backend.Card
import Texas.Backend.Player
import Data.Data (gcast1)
import GHC.Base (geWord)

-------------------------------------------------------------------------------

handleEvent :: GameWrapper -> BrickEvent Name Event -> EventM Name (Next GameWrapper)
handleEvent g (VtyEvent e) = case e of
    Vty.EvKey Vty.KEsc        [] -> halt g
    Vty.EvMouseDown col row _ _  -> do
        extents <- map extentName <$> findClickedExtents (col, row)
        case extents of 
            [Act Fold]      -> continue $ tryAction g Fold
            [Act Pass]      -> continue $ tryAction g Pass
            [Act (Add 2)]   -> continue $ tryAction g (Add 2)
            [Next]          -> continue $ doNext g
            _               -> continue g
    _                            -> continue g 

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [( attrName "card"      , fg Vty.white ),
      ( attrName "underline" , withStyle defAttr underline), 
      ( attrName "bold"      , withStyle defAttr bold)
     ]

-- App <application state> <event> <resource name>
app :: App GameWrapper Event Name
app = App { appDraw         = drawUI          -- s -> [Widget n]
          , appChooseCursor = showFirstCursor -- s -> [CursorLocation n] 
                                              --   -> Maybe (CursorLocation n)
          , appHandleEvent  = handleEvent     -- s -> BrickEvent n e 
                                              --   -> EventM n (Next s)
          , appStartEvent   = return          -- s -> EventM n s
          , appAttrMap      = const aMap      -- s -> AttrMap
          }

client :: IO ()
client = do
  let buildVty = do
        v <- Vty.mkVty =<< Vty.standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        return v
  initVty <- buildVty

  void $ customMain initVty buildVty Nothing app (newGameState 4 100 0 0)


module Texas.Driver.Entry 
(
    client
)
where

import Control.Monad                (void)
import Graphics.Vty as Vty

import Brick.AttrMap 
import Brick.Main
import Brick.Types 
import Brick.Util 

import Texas.Driver.Render    (drawUI)
import Texas.Driver.GameWrapper
import Texas.Backend.Game

-------------------------------------------------------------------------------

handleEvent :: GameWrapper -> BrickEvent Name Event -> EventM Name (Next GameWrapper)
handleEvent gw (VtyEvent e) = case e of
    Vty.EvKey Vty.KEsc        [] -> halt gw
    Vty.EvKey Vty.KEnter      [] -> continue $ doNext gw
    Vty.EvKey (Vty.KChar 'r') [] -> continue $ doReset gw
    Vty.EvMouseDown col row _ _  -> do
        extents <- map extentName <$> findClickedExtents (col, row)
        case extents of 
            [Next]      -> continue $ doNext gw
            [action]    -> continue $ tryAction gw action
            _           -> continue gw
    _                            -> continue gw 
handleEvent gw _ = continue gw

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [( attrName "redCard"   , fg Vty.brightRed),
      ( attrName "blackCard" , fg Vty.brightBlack),
      ( attrName "btnStyle"  , withBackColor defAttr Vty.white), 
      ( attrName "bold"      , withStyle defAttr bold),
      ( attrName "white"     , withForeColor defAttr Vty.white),
      ( attrName "red"       , withForeColor defAttr Vty.red), 
      ( attrName "green"     , withForeColor defAttr Vty.green)
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
  void $ customMain initVty buildVty Nothing app (newGameState 4 100 1 0)


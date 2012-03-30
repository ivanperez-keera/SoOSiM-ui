{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PackageImports #-}

import             Control.Monad.Trans
import             Data.Maybe
-- import             Debug.Trace
import "gloss-gtk" Graphics.Gloss.Interface.Pure.Game
import qualified   Graphics.UI.Gtk as Gtk
import             Graphics.UI.Gtk (AttrOp((:=)))

-- Local imports: basic types
import Graphics.MultiCoreStatus
import Graphics.Samples
import Graphics.PlainDiagram
import Graphics.Types (Name, Position, addPos, Size, multPos)

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagram2PlainDiagram
import Graphics.MultiCoreStatus2Diagram
import Graphics.PlainDiagram2Picture

main :: IO ()
main = do
  _ <- Gtk.initGUI

  window <- Gtk.windowNew
  vbox   <- Gtk.vBoxNew False 0
  ebox   <- Gtk.eventBoxNew 
  btn    <- Gtk.buttonNewWithLabel "Press me"

  Gtk.boxPackStart vbox ebox Gtk.PackNatural 0
  Gtk.boxPackStart vbox btn Gtk.PackNatural 0

  Gtk.set window [ Gtk.containerBorderWidth := 0, Gtk.containerChild := vbox ]

  window `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False

  Gtk.widgetShowAll window
  drawPic ebox
  Gtk.mainGUI

drawPic :: Gtk.ContainerClass a => a -> IO ()
drawPic e = 
  play (InWidget e (600, 600))
       white 100 state
       makePicture handleEvent stepWorld
 where state = State diagram 0


-- | The internal state is defined by the system's status and the time
-- elapsed since the last update
data State = State MultiCoreStatus Float

-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State dg _)
  = paintMultiCoreStatus dg

-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
  -- Finish drawing a line, and add it to the picture.
  | EventKey (MouseButton LeftButton) Up _ pt <- event
  , State dg t <- state
  = State (updateMenuClicks (unScale pt) dg) t

  | otherwise
  = state

updateMenuClicks :: Point -> MultiCoreStatus -> MultiCoreStatus
updateMenuClicks p st = 
   maybe st (`toggleVisibility` st) ns
 where ns = checkToggleVisibility p st

toggleVisibility :: [Name] -> MultiCoreStatus -> MultiCoreStatus
toggleVisibility [n] st = st { processingUnits = pu' }
 where pu  = processingUnits st
       pu' = map (toggleVisibilityPU n) pu
toggleVisibility _   st = st

toggleVisibilityPU :: Name -> ProcessingUnit -> ProcessingUnit
toggleVisibilityPU n pu = if n == unitName pu then pu' else pu
 where pu' = pu { unitStatus = toggleVisibilityS st }
       st  = unitStatus pu

toggleVisibilityS :: UnitStatus -> UnitStatus
toggleVisibilityS UnitCollapsed = UnitExpanded
toggleVisibilityS UnitExpanded  = UnitCollapsed
toggleVisibilityS UnitIgnored   = UnitIgnored

checkToggleVisibility :: Point -> MultiCoreStatus -> Maybe [Name]
checkToggleVisibility p st = listToMaybe l
 where l = mapMaybe (isMenuOfB p) boxes
       (PlainDiagram boxes _) = transformDiagram $ transformStatus st
  
isMenuOfB :: Position -> PBox -> Maybe [Name]
isMenuOfB p1 (PBox n p2 s _) = if isMenuOf p1 (p2, s) then Just [n] else Nothing
isMenuOfB p1 (PGroupBox n p2 s bs _)
 | isMenuOf p1 (p2,s) = Just [n]
 | otherwise          = fmap (n:) $ listToMaybe l
 where l = mapMaybe (isMenuOfB p1) bs

isMenuOf :: Position -> (Position, Size) -> Bool
isMenuOf (p11, p12) (p2, (_,th)) =
  (p11 >= p21 && p11 <= (p21 + w)
  && p12 >= p22 && p12 <= (p22 + h))
 where (p21, p22) = addPos p2 (0,th-20)
       (w,h)      = (20,20)

stepWorld :: Float -> State -> State
stepWorld n (State dg t)
 | t' > 0.9  = State dg' (t' - 0.9)
 | otherwise = State dg t'
 where dg' = toggleStatus ("PU1", "P2") dg
       t'  = n + t

toggleStatus :: (Name, Name) -> MultiCoreStatus -> MultiCoreStatus
toggleStatus qname (MultiCoreStatus ps ms) = MultiCoreStatus ps' ms
 where ps' = map (toggleStatusPU qname) ps

toggleStatusPU :: (Name, Name) -> ProcessingUnit -> ProcessingUnit
toggleStatusPU (pName, cName) pu
 | unitName pu == pName = ProcessingUnit pName us' st
 | otherwise            = pu
 where us' = map (toggleStatusRE cName) (unitElements pu)
       st  = unitStatus pu

toggleStatusRE :: Name -> RunningElement -> RunningElement
toggleStatusRE n re
 | elementName re == n = re { elementState = toggleStatusS (elementState re) }
 | otherwise           = re

toggleStatusS :: ElementState -> ElementState
toggleStatusS Active  = Waiting
toggleStatusS Waiting = Idle
toggleStatusS Idle    = Active

paintMultiCoreStatus :: MultiCoreStatus -> Picture
paintMultiCoreStatus =
  scale progScale progScale .  paintDiagram . transformDiagram . transformStatus

unScale :: Point -> Point
unScale p = multPos p (1 / progScale, 1 / progScale)

progScale :: Float
progScale = 0.5

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
module View.InitAnimationArea where

-- External imports
import             Data.CBMVar
import             Data.Maybe
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game
import             Graphics.UI.Gtk hiding (Color, Point, Size, LeftButton)

-- Local imports
import View.Objects

-- Local imports: basic types
import Graphics.MultiCoreStatus
-- import Graphics.Samples
import Graphics.PlainDiagram
import Graphics.Types (Name, Position, addPos, Size, multPos)

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagram2PlainDiagram
import Graphics.MultiCoreStatus2Diagram
import Graphics.PlainDiagram2Picture

initialiseAnimationArea :: CBMVar MultiCoreStatus -> Builder -> IO ()
initialiseAnimationArea mcs bldr = drawPic mcs =<< viewport1 bldr

drawPic :: ContainerClass a => CBMVar MultiCoreStatus -> a -> IO ()
drawPic mcs e =
  playIO (InWidget e (800, 600))
       white 100 state
       (makePicture mcs) queueEvent (stepWorld mcs)
 where state = State []

-- | In the internal state we just keep the pending events
data State = State [Event]

-- | Convert our state to a picture.
makePicture :: CBMVar MultiCoreStatus -> State -> IO Picture
makePicture mcs _ = fmap paintMultiCoreStatus $ readCBMVar mcs

queueEvent :: Event -> State -> State
queueEvent event state
  -- Finish drawing a line, and add it to the picture.
  | EventKey (MouseButton LeftButton) Up _ _ <- event
  , State evs <- state
  = State (evs ++ [event])

  | otherwise
  = state

-- | Handle mouse click and motion events.
handleEvent :: Event -> MultiCoreStatus -> MultiCoreStatus
handleEvent event dg
  -- Finish drawing a line, and add it to the picture.
  | EventKey (MouseButton LeftButton) Up _ pt <- event
  = updateMenuClicks (unScale pt) dg

  | otherwise
  = dg

updateMenuClicks :: Point -> MultiCoreStatus -> MultiCoreStatus
updateMenuClicks p st = maybe st (`toggleVisibility` st) ns
 where ns = checkToggleVisibility p st

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

-- Process the event queue and return an empty state
stepWorld :: CBMVar MultiCoreStatus -> Float -> State -> IO State
stepWorld mcsRef _ (State evs) = do
  mapM_ (\ev -> modifyCBMVar mcsRef (return . handleEvent ev)) evs
  return (State [])

paintMultiCoreStatus :: MultiCoreStatus -> Picture
paintMultiCoreStatus =
  scale progScale progScale .  paintDiagram . transformDiagram . transformStatus

unScale :: Point -> Point
unScale p = multPos p (1 / progScale, 1 / progScale)

progScale :: Float
progScale = 0.5

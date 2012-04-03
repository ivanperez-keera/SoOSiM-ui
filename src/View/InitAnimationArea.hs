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
import Graphics.PlainDiagram
import Graphics.Types (Name, Position, addPos, Size, multPos, subPos)

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagram2PlainDiagram
import Graphics.MultiCoreStatus2Diagram
import Graphics.PlainDiagram2Picture
import Graphics.SimState2MultiCoreStatus
import SoOSiM.Types

type StVar = CBMVar St
type St = (MultiCoreStatus, Maybe SimState)

initialiseAnimationArea :: StVar -> Builder -> IO ()
initialiseAnimationArea mcs bldr = drawPic mcs =<< viewport1 bldr

drawPic :: ContainerClass a => StVar -> a -> IO ()
drawPic mcs e =
  playIO (InWidget e (800, 600))
       white 100 state
       (makePicture mcs) queueEvent (stepWorld mcs)
 where state = State []

-- | In the internal state we just keep the pending events
data State = State [Event]

-- | Convert our state to a picture.
makePicture :: StVar -> State -> IO Picture
makePicture st _ = fmap paint $ readCBMVar st
 where paint (mcs, ss) = case ss of
                          Nothing -> paintMultiCoreStatus mcs
                          Just s  -> paintMultiCoreStatus (updateFromSimState mcs s)

queueEvent :: Event -> State -> State
queueEvent event state
  -- Finish drawing a line, and add it to the picture.
  | EventKey (MouseButton LeftButton) Up _ _ <- event
  , State evs <- state
  = State (evs ++ [event])

  | otherwise
  = state

-- | Handle mouse click and motion events.
handleEvent :: Event -> St -> St
handleEvent event dg
  -- Finish drawing a line, and add it to the picture.
  | EventKey (MouseButton LeftButton) Up _ pt <- event
  = updateMenuClicks (unScale pt) dg

  | otherwise
  = dg

updateMenuClicks :: Point -> St -> St
updateMenuClicks p (st,s) = (st',s)
 where ns    = checkToggleVisibility p st
       st'   = maybe st'' (`toggleVisibility` st) ns
       st''  = setSelection (fromMaybe [] (checkSetSelection p st')) st

checkToggleVisibility :: Point -> MultiCoreStatus -> Maybe [Name]
checkToggleVisibility p st = listToMaybe l
 where l = mapMaybe (isMenuOfB p) boxes
       (PlainDiagram boxes _) = transformDiagram $ transformStatus st
  
checkSetSelection :: Point -> MultiCoreStatus -> Maybe [Name]
checkSetSelection p st = listToMaybe l
 where l = mapMaybe (isAreaOfB p) boxes
       (PlainDiagram boxes _) = transformDiagram $ transformStatus st
  
isMenuOfB :: Position -> PBox -> Maybe [Name]
isMenuOfB p1 (PBox n p2 s _) = if isMenuOf p1 (p2, s) then Just [n] else Nothing
isMenuOfB p1 (PGroupBox n p2 s bs _)
 | isMenuOf p1 (p2,s) = Just [n]
 | otherwise          = fmap (n:) $ listToMaybe l
 where l   = mapMaybe (isMenuOfB p1') bs
       p1' = subPos p1 p2

isMenuOf :: Position -> (Position, Size) -> Bool
isMenuOf (p11, p12) (p2, (_,th)) =
  (p11 >= p21 && p11 <= (p21 + w)
  && p12 >= p22 && p12 <= (p22 + h))
 where (p21, p22) = addPos p2 (0,th-20)
       (w,h)      = (20,20)

isAreaOfB :: Position -> PBox -> Maybe [Name]
isAreaOfB p1 (PBox n p2 s _) = if isAreaOf p1 (p2, s) then Just [n] else Nothing
isAreaOfB p1 (PGroupBox n p2 s bs _)
 | not (null l)        = fmap (n:) $ listToMaybe l
 | isAreaOf p1 (p2, s) = Just [n]
 | otherwise           = Nothing
 where l   = mapMaybe (isAreaOfB p1') bs
       p1' = subPos p1 p2

isAreaOf :: Position -> (Position, Size) -> Bool
isAreaOf p1@(p11, p12) d@((p21, p22), (w,h)) =
  (p11 >= p21 && p11 <= (p21 + w)
   && p12 >= p22 && p12 <= (p22 + h))
  && not (isMenuOf p1 d)

-- Process the event queue and return an empty state
stepWorld :: StVar -> Float -> State -> IO State
stepWorld mcsRef _ (State evs) = do
  mapM_ (\ev -> modifyCBMVar mcsRef (return .handleEvent ev)) evs
  return (State [])

paintMultiCoreStatus :: MultiCoreStatus -> Picture
paintMultiCoreStatus =
  scale progScale progScale .  paintDiagram . transformDiagram . transformStatus

unScale :: Point -> Point
unScale p = multPos p (1 / progScale, 1 / progScale)

progScale :: Float
progScale = 0.5

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
-- | Presents the SimState to the user and updates it with the input events
module View.InitAnimationArea where

-- External imports
import             Data.CBMVar
import             Data.Maybe
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game
import             Graphics.UI.Gtk hiding (Color, Point, Size, LeftButton)

-- Local imports
import View.Objects
import SoOSiM.Types

-- Local imports: basic types
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types (Name, Position, addPos, Size, multPos, subPos)
import Graphics.Diagrams.Positioned.PositionedDiagram

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagrams.Transformations.Diagram2PositionedDiagram
import Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram
import Graphics.Diagrams.Transformations.PositionedDiagram2Picture
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus

-- Auxiliary types. We use an MVar with callbacks to communicate
-- with the rest of the program
type SimGlVar = CBMVar SimGlSt
type SimGlSt = (MultiCoreStatus, SimState)

-- | Initialises the opengl area with a picture
initialiseAnimationArea :: SimGlVar -> Builder -> IO ()
initialiseAnimationArea mcs bldr = drawPic mcs =<< viewport1 bldr

-- | Initialises the gloss animation
drawPic :: ContainerClass a => SimGlVar -> a -> IO ()
drawPic mcs e =
  playIO (InWidget e (800, 600))
       white 100 state
       (makePicture mcs) queueEvent (stepWorld mcs)
 where state = State []

-- | In the gloss internal state we just keep the pending events
data State = State [Event]

-- | Convert our state to a picture.
makePicture :: SimGlVar -> State -> IO Picture
makePicture st _ = fmap paint $ readCBMVar st
 where paint = paintMultiCoreStatus . uncurry updateFromSimState
 
-- | Transform the abstract status into a picture
paintMultiCoreStatus :: MultiCoreStatus -> Picture
paintMultiCoreStatus =
  scale progScale progScale .  paintDiagram . transformDiagram . transformStatus

-- | Queues an input event into the state's event queue
-- to be processed later
queueEvent :: Event -> State -> State
queueEvent event state
  -- Finish drawing a line, and add it to the picture.
  | EventKey (MouseButton LeftButton) Up _ _ <- event
  , State evs <- state
  = State (evs ++ [event])

  | otherwise
  = state

-- Process the event queue and return an empty state
stepWorld :: SimGlVar -> Float -> State -> IO State
stepWorld mcsRef _ (State evs) = do
  mapM_ (\ev -> modifyCBMVar mcsRef (return .handleEvent ev)) evs
  return (State [])

-- | Handle mouse click and motion events.
handleEvent :: Event -> SimGlSt -> SimGlSt
handleEvent event st
  -- Queue event
  | EventKey (MouseButton LeftButton) Up _ pt <- event
  = handleClicks (unScale pt) st

  | otherwise
  = st

-- | Process clicks in the boxes or menu icons
handleClicks :: Point -> SimGlSt -> SimGlSt
handleClicks p (st,s) = (st',s)
 where -- Expand/collapse when necessary
       st'   = maybe st'' (`toggleVisibility` st) ns
       -- Update selection
       st''  = st { selection = fromMaybe [] ss }
       ns    = checkToggleVisibility p st
       ss    = checkSetSelection p st

-- | Returns the qualified name of the box who's visibility
-- must be toggled (if any)
checkToggleVisibility :: Point -> MultiCoreStatus -> Maybe [Name]
checkToggleVisibility p st = listToMaybe l
 where l = mapMaybe (isMenuOfB p) boxes
       (PositionedDiagram boxes _) = transformDiagram $ transformStatus st
  
-- | Returns the qualified name of the box that the user
-- selected (if any)
checkSetSelection :: Point -> MultiCoreStatus -> Maybe [Name]
checkSetSelection p st = listToMaybe l
 where l = mapMaybe (isAreaOfB p) boxes
       (PositionedDiagram boxes _) = transformDiagram $ transformStatus st
  
-- | Returns the qualified name of the box who's menu
-- icon is in the given position (if any)
isMenuOfB :: Position -> PBox -> Maybe [Name]
isMenuOfB _  (PBox _ __ _ _) = Nothing
isMenuOfB p1 (PGroupBox n p2 s bs _ _)
 | isMenuOf p1 (p2,s) = Just [n]
 | otherwise          = fmap (n:) $ listToMaybe l
 where l   = mapMaybe (isMenuOfB p1') bs
       p1' = subPos p1 p2

-- | Returns True if the given position is in the menu
-- icon area of a box with the given dimensions
isMenuOf :: Position -> (Position, Size) -> Bool
isMenuOf (p11, p12) (p2, (_,th)) =
  (  p11 >= p21 && p11 <= p21 + w
  && p12 >= p22 && p12 <= p22 + h)
 where (p21, p22) = addPos p2 (0, th - 20)
       (w,h)      = (20, 20)

-- | Returns the qualified name of a box that is in
-- the given position
isAreaOfB :: Position -> PBox -> Maybe [Name]
isAreaOfB p1 (PBox n p2 s _) = if isAreaOf p1 (p2, s) then Just [n] else Nothing
isAreaOfB p1 (PGroupBox n p2 s bs _ _)
 | not (null l)        = fmap (n:) $ listToMaybe l
 | isAreaOf p1 (p2, s) = Just [n]
 | otherwise           = Nothing
 where l   = mapMaybe (isAreaOfB p1') bs
       p1' = subPos p1 p2

-- | Returns True if the given position is in the
-- area of a box with the given dimensions
isAreaOf :: Position -> (Position, Size) -> Bool
isAreaOf p1@(p11, p12) d@((p21, p22), (w,h)) =
  (p11 >= p21 && p11 <= (p21 + w)
   && p12 >= p22 && p12 <= (p22 + h))
  && not (isMenuOf p1 d)

-- | Unscales a point (adjusts value from user input dimensions to gloss
-- dimensions)
unScale :: Point -> Point
unScale p = multPos p (1 / progScale, 1 / progScale)

-- | The default scale
progScale :: Float
progScale = 0.5

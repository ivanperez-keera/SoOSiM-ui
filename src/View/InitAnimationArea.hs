{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
-- | Presents the SimState to the user and updates it with the input events
module View.InitAnimationArea where

-- External imports
import             Data.CBMVar
import             Data.Maybe
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game
import "gloss-gtk" Graphics.Gloss.Interface.IO.Animate
import             Graphics.UI.Gtk hiding (Color, Point, Size, LeftButton, RightButton)

-- Local imports
import View.Objects
import SoOSiM.Types

-- Local imports: basic types
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types (Name, Position, addPos, Size, multPos, subPos, BoxDescription)
import Graphics.Diagrams.Positioned.PositionedDiagram
import Graphics.Gloss.AdvancedShapes.Boxes

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagrams.Transformations.Diagram2PositionedDiagram
import Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram
import Graphics.Diagrams.Transformations.PositionedDiagram2Picture
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus

-- Auxiliary types. We use an MVar with callbacks to communicate
-- with the rest of the program
type SimGlVar  = CBMVar SimGlSt
type SimGlSt   = (MultiCoreStatus, SimState, ViewState, [Name])

type ViewState = (Float, Point)

initialViewState = (0.5, (-400, -100))

-- | Initialises the opengl area with a picture
initialiseAnimationArea :: SimGlVar -> Builder -> IO ()
initialiseAnimationArea mcs bldr = do
  vp <- viewport1 bldr
  ev <- eventbox1 bldr

  -- Paint animation inside viewport
  drawPic mcs vp

  -- Paint thumbnail inside eventbox with the viewport size for reference
  drawThumb mcs ev vp

-- | Initialises the gloss animation
drawPic :: ContainerClass a => SimGlVar -> a -> IO ()
drawPic mcs e =
  playIO (InWidget e (800, 600))
       white 100 state
       (makePicture mcs) queueEvent (stepWorld mcs)
 where state = State [] (fst initialViewState) (snd initialViewState) Nothing

-- | Draws a thumbnail of the main animation
drawThumb :: (ContainerClass a, ContainerClass b) => SimGlVar -> a -> b -> IO()
drawThumb mcs e be =
  animateIO (InWidget e (200, 150)) white (makeThumbnail (widgetGetSize be) mcs)

-- | In the gloss internal state we just keep the pending events
--   and the current scaling
data State = State [Event] Float Point (Maybe Point)

-- | Convert our state to a picture.
makePicture :: SimGlVar -> State -> IO Picture
makePicture st oldSt = do
  st'  <- readCBMVar st
  mcs' <- updateFromSimState (fst4 st') (snd4 st')
  return $ paintMultiCoreStatus sc orig mcs'
 where (State _ sc orig _) = oldSt

-- | Convert our state to a smaller thumbnail 
makeThumbnail :: IO (Int, Int) -> SimGlVar -> a -> IO Picture
makeThumbnail getSz st _ = do
  st' <- readCBMVar st
  sz  <- getSz
  return $ Pictures [ paintMultiCoreStatus thumbScale thumbCoords $ fst4 st'
                    , translate thumbX thumbY $ paintZoomBox (trd4 st') sz
                    ]

-- | Default thumbnail zoom level
thumbScale :: Float
thumbScale = 0.05

-- | Thumbnail base coords
thumbCoords :: Point
thumbCoords = (thumbX, thumbY)

-- | Thumbnail base X coord
thumbX :: Float
thumbX = (-90)

-- | Thumbnail base Y coord
thumbY :: Float
thumbY = 0

-- Paints the zoom box for a given scale, origin and container size
paintZoomBox :: (Float, Point) -> (Int, Int) -> Picture
paintZoomBox o (w',h') =
   box ((p1 * thumbScale, p2 * thumbScale), (w * thumbScale, h * thumbScale))
 where ((p1,p2),(w,h)) = zoomBoxDescription o sz
       sz              = (fromIntegral w', fromIntegral h')

-- | Builds the box description for the zoom box from the scale, origin and container size
zoomBoxDescription :: (Float, Point) -> (Float, Float) -> BoxDescription
zoomBoxDescription (s, (p1, p2)) (w, h) = ((p1'/s, p2'/s), (w / s, h / s))
 where p1' = - (w / 2 + p1)
       p2' = - (h / 2 + p2)
 
-- | Transform the abstract status into a picture
paintMultiCoreStatus :: Float -> Point -> MultiCoreStatus -> Picture
paintMultiCoreStatus progScale orig =
  uncurry translate orig . scale progScale progScale . paintDiagram . transformDiagram . transformStatus

-- | Zooms in a state with a specific zoom
zoomWith :: Float -> Point -> State -> State
zoomWith f (p1, p2) (State evs sc (o1,o2) no) = State evs (sc * f) o' no
 where p1' = p1 * (1 - f)
       p2' = p2 * (1 - f)
       o'  = (o1 * f + p1', o2 * f + p2')
 
-- | Queues an input event into the state's event queue
-- to be processed later
queueEvent :: Event -> State -> State
queueEvent event state
  -- Adds a click event to the event queue
  | EventKey (MouseButton LeftButton) Up m p <- event
  , State evs sc o no <- state
  = let p'     = unScale sc $ subPos p o 
        event' = EventKey (MouseButton LeftButton) Up m p'
    in State (evs ++ [event']) sc o no

  -- Only Down presses will be received for double clicks
  | EventKey (MouseButton LeftButtonDouble) Down m p <- event
  , State evs sc o no <- state
  = let p'     = unScale sc $ subPos p o 
        event' = EventKey (MouseButton LeftButtonDouble) Up m p'
    in State (evs ++ [event']) sc o no

  -- Zoom in
  | EventKey (MouseButton WheelUp) Down _ p <- event
  = zoomWith 0.9 p state

  -- Zoom out
  | EventKey (MouseButton WheelDown) Down _ p <- event
  = zoomWith 1.1 p state

  -- Start moving
  | EventKey (MouseButton RightButton) Down _ p <- event
  , State evs sc o _ <- state
  = State evs sc o (Just p)

  -- Finish moving
  | EventKey (MouseButton RightButton) Up _ _ <- event
  , State evs sc p _ <- state
  = State evs sc p Nothing

  -- Keep moving
  | EventMotion p <- event
  , State evs sc o (Just p') <- state
  = State evs sc (addPos o (subPos p p')) (Just p)

  | otherwise
  = state

-- Process the event queue and return an empty state
stepWorld :: SimGlVar -> Float -> State -> IO State
stepWorld mcsRef _ (State evs sc o no) = do
  mapM_ (\ev -> modifyCBMVar mcsRef (return . handleEvent sc ev)) evs
  modifyCBMVar mcsRef (\(a,b,c,s) -> return (a,b,(sc,o),s)) 
  return (State [] sc o no)

-- | Handle mouse click and motion events.
handleEvent :: Float -> Event -> SimGlSt -> SimGlSt
handleEvent sc event st
  -- Queue event
  | EventKey (MouseButton LeftButton) Up _ pt <- event
  = handleClicks pt st

  | EventKey (MouseButton LeftButtonDouble) Up _ pt <- event
  = handleDoubleClicks pt st

  | otherwise
  = st

-- | Process clicks in the boxes or menu icons
handleClicks :: Point -> SimGlSt -> SimGlSt
handleClicks p (st,s,v,op) = (st',s,v,op)
 where -- Expand/collapse when necessary
       st'   = maybe st (`toggleVisibility` st) ns
       -- Update selection
       ns    = checkToggleVisibility p st

-- | Process double clicks in component boxes
handleDoubleClicks :: Point -> SimGlSt -> SimGlSt
handleDoubleClicks p (st,s,v,_) = (st',s,v,fromMaybe [] ss)
 where ss = case checkSetSelection p st of
             Just [x,y] -> Just [x,y]
             _          -> Nothing
       ns = checkToggleVisibility p st
       st' | isNothing ns = st { selection = fromMaybe [] ss }
           | otherwise    = st

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
isMenuOfB _  (PBox _ __ _ _ _) = Nothing
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
isAreaOfB p1 (PBox n k p2 s _) = if isAreaOf p1 (p2, s) then Just [n] else Nothing
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
unScale :: Float -> Point -> Point
unScale progScale p = multPos p (1 / progScale, 1 / progScale)

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

trd4 :: (a,b,c,d) -> c
trd4 (_,_,c,_) = c

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards  #-}
-- | Presents the SimState to the user and updates it with input events
module View.InitAnimationArea 
        (SimGlVar, SimGlSt, initialViewState, initialiseAnimationArea)
       where

-- External imports
import             Data.CBMVar
import             Data.Maybe
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game
import "gloss-gtk" Graphics.Gloss.Interface.IO.Animate
import             Graphics.UI.Gtk hiding ( Color, Point, Size
                                          , LeftButton, RightButton
                                          )

-- Local imports
import Config.Config
import Config.Preferences
import Data.Tuple4
import Data.History
import Model.SystemStatus
import SoOSiM.Types
import View.Objects

-- Local imports: basic types
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types ( Name, Position, BoxDescription
                               , addPos, Size, subPos, inArea, unScale
                               )
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
type SimGlSt   = (SystemStatus, SimState, ViewState, [Name])

-- | Initialises the opengl area with a picture
initialiseAnimationArea :: Config -> SimGlVar -> Builder -> IO ()
initialiseAnimationArea cfg mcs bldr = do
  vp <- animationViewport bldr
  ev <- overviewEventBox bldr

  -- Paint thumbnail inside eventbox with the viewport size for reference
  drawThumb cfg mcs ev vp

  -- Paint animation inside viewport
  drawPic cfg mcs vp

-- | Initialises the gloss animation
drawPic :: ContainerClass a => Config -> SimGlVar -> a -> IO ()
drawPic cfg mcs e =
  playIO (InWidget e initialAnimationSize) white fps state
         (makePicture cfg mcs) queueEvent (stepWorld mcs)
 where state = State [] (fst initialViewState) (snd initialViewState) Nothing
       fps   = 100

-- | Draws a thumbnail of the main animation
drawThumb :: (ContainerClass a, ContainerClass b) => Config -> SimGlVar -> a -> b -> IO()
drawThumb cfg mcs e be =
  animateIO (InWidget e initialThumbnailSize)
    white (makeThumbnail cfg (widgetGetSize be) mcs)

-- | In the gloss internal state we just keep the pending events
--   and the current scaling
data State = State [Event] Float Point (Maybe Point)

-- * Auxiliary Picture Generators

-- | Convert the state into a picture.
makePicture :: Config -> SimGlVar -> State -> IO Picture
makePicture cfg st oldSt = do
  st' <- readCBMVar st

  -- Calculate next multi-core status (if not already known)
  let hist = multiCoreStatus $ fst4 st'
  mcs' <- case future hist of
           [] -> updateFromSimState (historyPresent hist) (snd4 st')
           _  -> return (present hist)

  -- Update the multi core status
  let newSt = (fst4 st') { multiCoreStatus = hist { present = mcs' } }

  return $ paintMultiCoreStatus cfg sc orig newSt
 where (State _ sc orig _) = oldSt

-- | Convert our state to a smaller thumbnail 
makeThumbnail :: Config -> IO (Int, Int) -> SimGlVar -> a -> IO Picture
makeThumbnail cfg getSz st _ = do
  st' <- readCBMVar st
  sz  <- getSz
  -- Calculate next multi-core status (if not already known)
  let hist = multiCoreStatus $ fst4 st'
  mcs' <- case future hist of
           [] -> updateFromSimState (historyPresent hist) (snd4 st')
           _  -> return (present hist)

  -- Update the multi core status
  let newSt = (fst4 st') { multiCoreStatus = hist { present = mcs' } }

  return $ Pictures [ paintMultiCoreStatus cfg thumbScale thumbCoords newSt
                    , translate thumbX thumbY $ paintZoomBox (trd4 st') sz
                    ]

-- * Main picture step processing

-- Process the event queue and return an empty state
stepWorld :: SimGlVar -> Float -> State -> IO State
stepWorld mcsRef _ (State evs sc o no) = do
  mapM_ (\ev -> modifyCBMVar mcsRef (return . handleEvent sc ev)) evs
  modifyCBMVar mcsRef (\(a,b,_,s) -> return (a,b,(sc,o),s)) 
  return (State [] sc o no)

-- * Main picture event handlers
  
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
  = zoomWith stdZoomStep p state

  -- Zoom out
  | EventKey (MouseButton WheelDown) Down _ p <- event
  = zoomWith (1 / stdZoomStep) p state

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

  -- Moving over the diagram
  | EventMotion p <- event
  , State evs sc o Nothing <- state
  = let p'     = unScale sc $ subPos p o 
        event' = EventMotion p'
    in State (evs ++ [event']) sc o Nothing
  | otherwise
  = state

-- | Handle mouse click and motion events.
handleEvent :: Float -> Event -> SimGlSt -> SimGlSt
handleEvent _sc event st
  -- Queue event
  | EventKey (MouseButton LeftButton) Up _ pt <- event
  = handleClicks pt st

  | EventKey (MouseButton LeftButtonDouble) Up _ pt <- event
  = handleDoubleClicks pt st

  | EventMotion pt <- event
  = handleMouseOver pt st

  | otherwise
  = st

-- | Process clicks in the boxes or menu icons
handleClicks :: Point -> SimGlSt -> SimGlSt
handleClicks p (st,s,v,op) = (st',s,v,op)
 where -- Expand/collapse when necessary
       st'   = maybe st (updateCurrentStatus st . toggleVisibility) ns
       -- Update selection
       ns    = checkToggleVisibility p st

-- | Process double clicks in component boxes
handleDoubleClicks :: Point -> SimGlSt -> SimGlSt
handleDoubleClicks p (st,s,v,o) = (st',s,v,o)
 where ss = simpleBoxName =<< checkSetSelection p st -- Select simple boxes only
       ns = checkToggleVisibility p st
       st' | isNothing ns = st { selection = fromMaybe [] ss }
           | otherwise    = st

-- | Process moving the mouse over component boxes
handleMouseOver :: Point -> SimGlSt -> SimGlSt
handleMouseOver p (st,s,v,_) = (st,s,v,fromMaybe [] ss)
 where ss = simpleBoxName =<< checkSetSelection p st -- Hovering over simple boxes only

-- | Zooms in/out of a state with a specific zoom
zoomWith :: Float -> Point -> State -> State
zoomWith f (p1, p2) (State evs sc (o1,o2) no) = State evs (sc * f) o' no
 where p1' = p1 * (1 - f)
       p2' = p2 * (1 - f)
       o'  = (o1 * f + p1', o2 * f + p2')

-- | Returns the qualified name of the box who's visibility
-- must be toggled (if any)
checkToggleVisibility :: Point -> SystemStatus -> Maybe [Name]
checkToggleVisibility p st = listToMaybe $ mapMaybe (isMenuOfB p) boxes
 where (PositionedDiagram boxes _) = transformDiagram $ transformStatus defaultConfig st
  
-- | Returns the qualified name of the box that the user
-- selected (if any)
checkSetSelection :: Point -> SystemStatus -> Maybe [Name]
checkSetSelection p st = listToMaybe $ mapMaybe (isAreaOfB p) boxes
 where (PositionedDiagram boxes _) = transformDiagram $ transformStatus defaultConfig st
  
-- * Auxiliary gloss functions

-- | Transform the abstract status into a picture
paintMultiCoreStatus :: Config -> Float -> Point -> SystemStatus -> Picture
paintMultiCoreStatus cfg progScale orig =
  uncurry translate orig . scale progScale progScale .
    paintDiagram . transformDiagram . transformStatus cfg

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

-- * Auxiliary box geometry functions

-- | Returns the qualified name of the box who's menu
-- icon is in the given position (if any)
isMenuOfB :: Position -> PBox -> Maybe [Name]
isMenuOfB _  (PBox _ __ _ _ _) = Nothing
isMenuOfB p1 (PGroupBox n p2 s bs _ _)
 | isMenuOf p1 (p2,s) = Just [n]
 | otherwise          = fmap (n:) $ listToMaybe l
 where l   = mapMaybe (isMenuOfB p1') bs
       p1' = subPos p1 p2  -- p1 relative to p2

-- | Returns True if the given position is in the menu
-- icon area of a box with the given dimensions
isMenuOf :: Position -> (Position, Size) -> Bool
isMenuOf p1 (p2, (_,th)) = inArea p1 (p2', stdMenuBoxSize)
 where p2' = addPos p2 (0, th - (snd stdMenuBoxSize))

-- | Returns the qualified name of the box in the given position
isAreaOfB :: Position -> PBox -> Maybe [Name]
isAreaOfB p1 b
 | not (null l)         = fmap (n:) $ listToMaybe l
 | isAreaOf p1 (p2, sz) = Just [n]
 | otherwise            = Nothing
 where l   = mapMaybe (isAreaOfB p1') $ pboxSubBoxes b
       p1' = subPos p1 p2   -- p1 relative to p2
       n   = pboxName     b
       p2  = pboxPosition b
       sz  = pboxSize     b

-- | Returns True if the given position is in the
-- area of a box with the given dimensions
isAreaOf :: Position -> (Position, Size) -> Bool
isAreaOf p d = inArea p d && not (isMenuOf p d)

-- * Auxiliary Box functions

-- | Returns just the name unmodified if it's a simple box name
-- (PUName, ComponentName), otherwise returns Nothing
simpleBoxName :: [Name] -> Maybe [Name]
simpleBoxName n@[_,_] = Just n
simpleBoxName _       = Nothing
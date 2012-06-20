{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards  #-}
-- | Presents the SimState to the user and updates it with input events
module View.InitMainPictureArea
       where

-- External imports
import             Data.CBMVar
import             Data.Maybe
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game
import             Graphics.UI.Gtk (ContainerClass)

-- Local imports
import Config.Config
import Config.Preferences
import Graphics.Zoom
import Model.SystemStatus
import View.Animation

-- Local imports: basic types
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types ( Name, addPos, subPos, unScale )
import Graphics.Diagrams.Positioned.PositionedDiagram

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagrams.Transformations.Diagram2PositionedDiagram
import Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram

-- | Initialises the gloss animation
drawPic :: ContainerClass a => Config -> SimGLVar -> a -> IO ()
drawPic cfg mcs e =
  playIO (InWidget e initialAnimationSize) white fps state
         (makePicture cfg mcs) queueEvent (stepWorld mcs)
 where state = State [] (fst initialViewState) (snd initialViewState) Nothing
       fps   = 100

-- | Convert the state into a picture.
makePicture :: Config -> SimGLVar -> State -> IO Picture
makePicture cfg st oldSt = makeImage cfg st sc orig
 where (State _ sc orig _) = oldSt

-- * Main picture step processing

-- Process the event queue and return an empty state
stepWorld :: SimGLVar -> Float -> State -> IO State
stepWorld mcsRef _ (State evs sc o no) = do
  mapM_ (\ev -> modifyCBMVar mcsRef (return . handleEvent ev)) evs
  modifyCBMVar mcsRef (\state -> return (state { simGLViewState = (sc, o) }))
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
  , State evs sc o no <- state
  , (sc',o') <- zoomWith stdZoomStep p (sc, o)
  = State evs sc' o' no

  -- Zoom out
  | EventKey (MouseButton WheelDown) Down _ p <- event
  , State evs sc o no <- state
  , (sc',o') <- zoomWith (1/stdZoomStep) p (sc, o)
  = State evs sc' o' no

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
handleEvent :: Event -> SimGLState -> SimGLState
handleEvent event state
  -- Queue event
  | EventKey (MouseButton LeftButton) Up _ point <- event
  = handleClicks point state

  | EventKey (MouseButton LeftButtonDouble) Up _ point <- event
  = handleDoubleClicks point state

  | EventMotion point <- event
  = handleMouseOver point state

  | otherwise
  = state

-- | Process clicks in the boxes or menu icons
handleClicks :: Point -> SimGLState -> SimGLState
handleClicks p state = state { simGLSystemStatus = st' }
 where -- Expand/collapse when necessary
       st' = maybe st (updateCurrentStatus st . toggleVisibility) ns
       -- Check whether an item has been clicked
       ns  = checkToggleVisibility p st
       -- Old system status
       st = simGLSystemStatus state

-- | Process double clicks in component boxes
handleDoubleClicks :: Point -> SimGLState -> SimGLState
handleDoubleClicks p state = state { simGLSystemStatus = st' }
 where ss = simpleBoxName =<< checkSetSelection p st -- Select simple boxes only
       ns = checkToggleVisibility p st
       st = simGLSystemStatus state
       st' | isNothing ns = st { selection = fromMaybe [] ss }
           | otherwise    = st

-- | Process moving the mouse over component boxes
handleMouseOver :: Point -> SimGLState -> SimGLState
handleMouseOver p state = state { simGLSelection = fromMaybe [] ss }
 where ss = simpleBoxName =<< checkSetSelection p st -- Hovering over simple boxes only
       st = simGLSystemStatus state

-- | Returns the qualified name of the box who's visibility
-- must be toggled (if any)
checkToggleVisibility :: Point -> SystemStatus -> Maybe [Name]
checkToggleVisibility p st = anyMenu p $ transformDiagram $ transformStatus defaultConfig st
  
-- | Returns the qualified name of the box that the user
-- selected (if any)
checkSetSelection :: Point -> SystemStatus -> Maybe [Name]
checkSetSelection p st = anyArea p $ transformDiagram $ transformStatus defaultConfig st
  
-- | Returns just the name unmodified if it's a simple box name
-- (PUName, ComponentName), otherwise returns Nothing
simpleBoxName :: [Name] -> Maybe [Name]
simpleBoxName n@[_,_] = Just n
simpleBoxName _       = Nothing
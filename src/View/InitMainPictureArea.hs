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
import             Graphics.UI.Gtk (ContainerClass, Widget, toWidget)

-- Local imports
import Config.Config
import Config.Preferences
import Graphics.UI.Gtk.Display.GlossIO
import Graphics.Zoom
import Model.SystemStatus
import View.Animation
import Graphics.UI.Gtk.Display.SoOSiMState

-- Local imports: basic types
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types ( Name, addPos, subPos, unScale )
import Graphics.Diagrams.Positioned.PositionedDiagram

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagrams.Transformations.Diagram2PositionedDiagram
import Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram

drawPic :: Config -> IO SoOSiMState
drawPic cfg = do
  soosim <- soosimStateNew cfg
  -- gloss  <- glossIONewWithGame 0.5 (-400, -100) initialAnimationSize fps state
  --              (makePicture cfg mcs) queueEvent (stepWorld mcs)
  return soosim
 -- where state = (State [])
 --       fps   = 100

-- -- | Convert the state into a picture.
-- makePicture :: Config -> SimGLVar -> State -> IO Picture
-- makePicture cfg st oldSt = makeImage cfg st 1.0 (0,0)
-- 
-- -- * Main picture step processing
-- 
-- -- Process the event queue and return an empty state
-- stepWorld :: SimGLVar -> Float -> State -> IO State
-- stepWorld mcsRef _ (State evs) = do
--   mapM_ (\ev -> modifyCBMVar mcsRef (return . handleEvent ev)) evs
--   return (State [])
-- 
-- -- * Main picture event handlers
--   
-- -- | Queues an input event into the state's event queue
-- -- to be processed later
-- queueEvent :: Event -> State -> State
-- queueEvent event (State evs) = State (evs ++ [event])
-- 
-- -- | Handle mouse click and motion events.
-- handleEvent :: Event -> SimGLState -> SimGLState
-- handleEvent event state
--   -- Queue event
--   | EventKey (MouseButton LeftButton) Up _ point <- event
--   = handleClicks point state
-- 
--   | EventKey (MouseButton LeftButtonDouble) Up _ point <- event
--   = handleDoubleClicks point state
-- 
--   | EventMotion point <- event
--   = handleMouseOver point state
-- 
--   | otherwise
--   = state
-- 
-- -- | Process clicks in the boxes or menu icons
-- handleClicks :: Point -> SimGLState -> SimGLState
-- handleClicks p state = state { simGLSystemStatus = st' }
--  where -- Expand/collapse when necessary
--        st' = maybe st (updateCurrentStatus st . toggleVisibility) ns
--        -- Check whether an item has been clicked
--        ns  = checkToggleVisibility p st
--        -- Old system status
--        st = simGLSystemStatus state
-- 
-- -- | Process double clicks in component boxes
-- handleDoubleClicks :: Point -> SimGLState -> SimGLState
-- handleDoubleClicks p state = state { simGLSystemStatus = st' }
--  where ss = simpleBoxName =<< checkSetSelection p st -- Select simple boxes only
--        ns = checkToggleVisibility p st
--        st = simGLSystemStatus state
--        st' | isNothing ns = st { selection = fromMaybe [] ss }
--            | otherwise    = st
-- 
-- -- | Process moving the mouse over component boxes
-- handleMouseOver :: Point -> SimGLState -> SimGLState
-- handleMouseOver p state = state { simGLSelection = fromMaybe [] ss }
--  where ss = simpleBoxName =<< checkSetSelection p st -- Hovering over simple boxes only
--        st = simGLSystemStatus state
-- 
-- -- | Returns the qualified name of the box who's visibility
-- -- must be toggled (if any)
-- checkToggleVisibility :: Point -> SystemStatus -> Maybe [Name]
-- checkToggleVisibility p st = anyMenu p $ transformDiagram $ transformStatus defaultConfig st
--   
-- -- | Returns the qualified name of the box that the user
-- -- selected (if any)
-- checkSetSelection :: Point -> SystemStatus -> Maybe [Name]
-- checkSetSelection p st = anyArea p $ transformDiagram $ transformStatus defaultConfig st
--   
-- -- | Returns just the name unmodified if it's a simple box name
-- -- (PUName, ComponentName), otherwise returns Nothing
-- simpleBoxName :: [Name] -> Maybe [Name]
-- simpleBoxName n@[_,_] = Just n
-- simpleBoxName _       = Nothing

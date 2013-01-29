{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards  #-}
module Graphics.UI.Gtk.Display.SoOSiMState where

-- External imports
import             Control.Monad
import             Data.CBMVar
import             Data.Maybe
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game
import             Graphics.UI.Gtk (ObjectClass, WidgetClass)
import             System.Glib.Types
import             SoOSiM.Types hiding (State)

-- Local imports
import Config.Config
import Config.Preferences
import Data.History
import Graphics.UI.Gtk.Display.GlossIO
import Model.SystemStatus

-- Local imports: basic types
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types ( Name )
import Graphics.Diagrams.Positioned.PositionedDiagram

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagrams.Transformations.Diagram2PositionedDiagram
import Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
import Graphics.Diagrams.Transformations.PositionedDiagram2Picture

data SoOSiMState = SoOSiMState
  { soosimIO     :: GlossIO
  , soosimParams :: SoOSiMStateParams
  }

data SoOSiMStateParams = SoOSiMStateParams
  { soosimState     :: CBMVar (Maybe SimState)
  , soosimSelection :: CBMVar (Maybe [Name])
  , soosimMCS       :: CBMVar (Maybe MultiCoreStatus)
  }

instance WidgetClass SoOSiMState
instance ObjectClass SoOSiMState
instance GObjectClass SoOSiMState where
  toGObject = toGObject . soosimIO 
  unsafeCastGObject x = SoOSiMState (unsafeCastGObject x) undefined

soosimStateNew :: Config -> IO SoOSiMState
soosimStateNew cfg = do
  -- glossIO <- glossIONewWithGame 
  gloss <- glossIONew
  st    <- newCBMVar Nothing
  sel   <- newCBMVar Nothing
  mcs   <- newCBMVar Nothing
  let soosim = SoOSiMState gloss (SoOSiMStateParams st sel mcs)

  glossIOStartGame gloss 0.5 (-400, -100) initialAnimationSize fps state
                   (makePicture cfg soosim) queueEvent (stepWorld soosim)
  return soosim
 where state = State []
       fps   = 100

-- | Convert the state into a picture.
makePicture :: Config -> SoOSiMState -> State -> IO Picture
makePicture cfg st _ = makeImage' cfg st

-- | Convert the state into a picture.
makeImage' :: Config -> SoOSiMState -> IO Picture
makeImage' cfg soosim = do
  st  <- soosimGetSimState soosim
  sel <- soosimGetSelection soosim
  mcs <- soosimGetMCS soosim
  if (isNothing st || isNothing sel || isNothing mcs)
   then return $ Pictures []
   else do
    let hist = historyNew (fromJust mcs)
        systemSt     = SystemStatus hist (fromJust sel)
    mcs' <- updateFromSimState (historyPresent hist) (fromJust st)
    -- Update the multi core status
    let newSt = systemSt { multiCoreStatus = hist { present = mcs' } }

    return $ paintMultiCoreStatus cfg newSt

-- * Main picture step processing

-- Process the event queue and return an empty state
stepWorld :: SoOSiMState -> Float -> State -> IO State
stepWorld soosim _ (State evs) = do
  -- Build basic status
  st  <- soosimGetSimState soosim
  sel <- soosimGetSelection soosim
  mcs <- soosimGetMCS soosim
  when (isJust st && isJust sel && isJust mcs) $ do
    let hist = historyNew (fromJust mcs)
        systemSt     = SystemStatus hist (fromJust sel)
        staticStatus = SoOSiMStaticStatus (fromJust st)
                                          systemSt
                                          (fromJust sel)
    -- Calculate status
    let newStaticStatus = foldr handleEvent staticStatus evs

    -- Update status
    -- soosimSetSimState soosim (soosimStaticState newStaticStatus)
    soosimSetSelection soosim $ Just (soosimStaticSel newStaticStatus)
    soosimSetMCS soosim $ Just (present $ multiCoreStatus $ soosimStaticStatus newStaticStatus)

  -- mapM_ (\ev -> modifyCBMVar mcsRef (return . handleEvent ev)) evs
  
  return (State [])

-- * Main picture event handlers
  
-- | Queues an input event into the state's event queue
-- to be processed later
queueEvent :: Event -> State -> State
queueEvent event (State evs) = State (event : evs)

data SoOSiMStaticStatus = SoOSiMStaticStatus
  { soosimStaticState  :: SimState
  , soosimStaticStatus :: SystemStatus
  , soosimStaticSel    :: [Name]
  -- , soosimStaticMCS    :: MultiCoreStatus
  }

-- | Handle mouse click and motion events.
handleEvent :: Event -> SoOSiMStaticStatus -> SoOSiMStaticStatus -- SimGLState -> SimGLState
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
handleClicks :: Point -> SoOSiMStaticStatus -> SoOSiMStaticStatus
handleClicks p state = state { soosimStaticStatus = st' }
 where -- Expand/collapse when necessary
       st' = maybe st (updateCurrentStatus st . toggleVisibility) ns
       -- Check whether an item has been clicked
       ns  = checkToggleVisibility p st
       -- Old system status
       st = soosimStaticStatus state

-- | Process double clicks in component boxes
handleDoubleClicks :: Point -> SoOSiMStaticStatus -> SoOSiMStaticStatus
handleDoubleClicks p state = state { soosimStaticStatus = st' }
 where ss = simpleBoxName =<< checkSetSelection p st -- Select simple boxes only
       ns = checkToggleVisibility p st
       st = soosimStaticStatus state
       st' | isNothing ns = st { selection = fromMaybe [] ss }
           | otherwise    = st

-- | Process moving the mouse over component boxes
handleMouseOver :: Point -> SoOSiMStaticStatus -> SoOSiMStaticStatus
handleMouseOver p state = state { soosimStaticSel = fromMaybe [] ss }
 where ss = simpleBoxName =<< checkSetSelection p st -- Hovering over simple boxes only
       st = soosimStaticStatus state

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

asGlossIO :: SoOSiMState -> GlossIO
asGlossIO = soosimIO

soosimOnStateChanged     :: SoOSiMState -> IO () -> IO ()
soosimOnStateChanged = installCallbackCBMVar . soosimState . soosimParams

soosimOnSelectionChanged :: SoOSiMState -> IO () -> IO ()
soosimOnSelectionChanged = installCallbackCBMVar . soosimSelection . soosimParams

-- soosimUpdateSimState :: SoOSiMState -> SimState -> IO ()
-- soosimUpdateSimState = undefined

soosimSetSimState    :: SoOSiMState -> Maybe SimState -> IO ()
soosimSetSimState    = writeCBMVar . soosimState . soosimParams

soosimGetSimState    :: SoOSiMState -> IO (Maybe SimState)
soosimGetSimState    = readCBMVar . soosimState . soosimParams

soosimSetSelection   :: SoOSiMState -> Maybe [Name] -> IO()
soosimSetSelection   = writeCBMVar . soosimSelection . soosimParams

soosimGetSelection   :: SoOSiMState -> IO (Maybe [Name])
soosimGetSelection   = readCBMVar . soosimSelection . soosimParams

soosimSetMCS   :: SoOSiMState -> Maybe MultiCoreStatus -> IO()
soosimSetMCS   = writeCBMVar . soosimMCS . soosimParams

soosimGetMCS         :: SoOSiMState -> IO (Maybe MultiCoreStatus)
soosimGetMCS         = readCBMVar . soosimMCS . soosimParams

instance GlossIOClass SoOSiMState where
 -- glossIOSetSensitive :: a -> Bool -> IO ()
 glossIOSetSensitive = glossIOSetSensitive . asGlossIO

 -- glossIOSetZoom :: a -> Float -> IO ()
 glossIOSetZoom = glossIOSetZoom . asGlossIO

 -- glossIOGetZoom :: a -> IO Float
 glossIOGetZoom = glossIOGetZoom . asGlossIO

 -- glossIOSetOrig :: a -> G.Point -> IO ()
 glossIOSetOrig = glossIOSetOrig . asGlossIO

 -- glossIOAddToOrig :: a -> G.Point -> IO ()
 glossIOAddToOrig = glossIOAddToOrig . asGlossIO

 -- glossIOGetOrig :: a -> IO G.Point
 glossIOGetOrig = glossIOGetOrig . asGlossIO

 -- glossIOGetPicture :: a -> IO (Maybe Picture)
 glossIOGetPicture = glossIOGetPicture . asGlossIO

 -- glossIOOnZoomChange :: a -> IO () -> IO ()
 glossIOOnZoomChange = glossIOOnZoomChange . asGlossIO

 -- glossIOOnOrigChange :: a -> IO () -> IO ()
 glossIOOnOrigChange = glossIOOnOrigChange . asGlossIO

-- | Transform the abstract status into a picture
paintMultiCoreStatus :: Config -> SystemStatus -> Picture
paintMultiCoreStatus cfg = paintDiagram . transformDiagram . transformStatus cfg

-- | In the gloss internal state we just keep the pending events
--   and the current scaling
data State = State [Event]

{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE PatternGuards   #-}
-- | Presents the SimState to the user and updates it with input events
module View.Animation where

-- External imports
import             Data.CBMVar
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game

-- Local imports
import Config.Config
import Config.Preferences
import Data.History
import Model.SystemStatus
import SoOSiM.Types

-- Local imports: basic types
import Graphics.Diagrams.Types (Name)

-- Local imports: transformation functions: Status ~> Picture
import Graphics.Diagrams.Transformations.Diagram2PositionedDiagram
import Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram
import Graphics.Diagrams.Transformations.PositionedDiagram2Picture
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus

-- Auxiliary types. We use an MVar with callbacks to communicate
-- with the rest of the program
type SimGLVar   = CBMVar SimGLState
data SimGLState = SimGLState
  { simGLSystemStatus :: SystemStatus
  , simGLSimState     :: SimState
  , simGLViewState    :: ViewState
  , simGLSelection    :: [Name]
  }

-- | In the gloss internal state we just keep the pending events
--   and the current scaling
data State = State [Event] Float Point (Maybe Point)

-- | Convert the state into a picture.
makeImage :: Config -> SimGLVar -> Float -> Point -> IO Picture
makeImage cfg st sc orig = do
  st' <- readCBMVar st

  -- Calculate next multi-core status (if not already known)
  let hist = multiCoreStatus $ simGLSystemStatus st'
  mcs' <- case future hist of
           [] -> updateFromSimState (historyPresent hist) (simGLSimState st')
           _  -> return (present hist)

  -- Update the multi core status
  let newSt = (simGLSystemStatus st') { multiCoreStatus = hist { present = mcs' } }

  return $ paintMultiCoreStatus cfg sc orig newSt

-- | Transform the abstract status into a picture
paintMultiCoreStatus :: Config -> Float -> Point -> SystemStatus -> Picture
paintMultiCoreStatus cfg progScale orig =
  uncurry translate orig . scale progScale progScale .
    paintDiagram . transformDiagram . transformStatus cfg
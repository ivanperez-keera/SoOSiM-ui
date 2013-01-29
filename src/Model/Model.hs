-- | The internal system's model
module Model.Model where

import SoOSiM.Types

import Graphics.Diagrams.Types (Name)
import Model.SystemStatus

-- | The system's model can be described by giving:
--
--  * The simulation parameters (speed, status)
--  * The current view mode (fullscreen, windowed)
data Model = Model
  { speed      :: Float
  , status     :: Status
  , fullscreen :: Bool
  , simState   :: Maybe SimGLState
  }

data SimGLState = SimGLState
  { simGLSystemStatus :: SystemStatus
  , simGLSimState     :: SimState
  , simGLInitialState :: IO SimState
  , simGLSelection    :: [Name]
  }

instance Eq SimGLState where
  _ == _ = False

-- | A simulation can be running, paused or halted
data Status = Running
            | SlowRunning
            | Paused
            | Stopped
 deriving (Eq, Show)

-- | Initially, the simulation is running at 1 step per second
-- in windowed mode
emptyBM :: Model
emptyBM = Model { speed      = 1
                , status     = Paused
                , fullscreen = False
                , simState   = Nothing
                }

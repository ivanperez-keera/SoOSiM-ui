-- | The internal system's model
module Model.Model where

-- | The system's model can be described by giving:
-- 
--  * The simulation parameters (speed, status)
--  * The current view mode (fullscreen, windowed)
data Model = Model
  { speed      :: Float
  , status     :: Status
  , fullscreen :: Bool
  }


-- | A simulation can be running, paused or halted
data Status = Running
            | Paused
            | Stopped
 deriving (Eq, Show)

-- | Initially, the simulation is running at 1 step per second
-- in windowed mode
emptyBM :: Model
emptyBM = Model { speed      = 1
                , status     = Paused
                , fullscreen = False
                }

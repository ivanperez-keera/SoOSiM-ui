module Model.Model where

data Model = Model
  { speed      :: Float
  , status     :: Status
  , fullscreen :: Bool
  }

emptyBM :: Model
emptyBM = Model { speed      = 1
                , status     = Running
                , fullscreen = False
                }

data Status = Running
            | Paused
            | Stopped
 deriving (Eq, Show)

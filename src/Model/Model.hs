module Model.Model where

data Model = Model
  { speed  :: Float
  , status :: Status
  }

emptyBM :: Model
emptyBM = Model { speed  = 1
                , status = Running
                }

data Status = Running
            | Stopped
 deriving (Eq)

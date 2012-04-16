module Model.SystemStatus where

import Data.History

-- Internal imports
-- FIXME: these should be in the model
import Graphics.Diagrams.Types
import Graphics.Diagrams.MultiCoreStatus

data SystemStatus = SystemStatus
  { multiCoreStatus :: History MultiCoreStatus
  , selection       :: [Name]
  }

updateCurrentStatus :: SystemStatus -> (MultiCoreStatus -> MultiCoreStatus) -> SystemStatus
updateCurrentStatus s f = s { multiCoreStatus = mcs { present = f cur} }
  where mcs = multiCoreStatus s
        cur = present mcs

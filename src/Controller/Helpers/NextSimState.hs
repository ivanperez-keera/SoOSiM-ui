module Controller.Helpers.NextSimState where

import Graphics.MultiCoreStatus
import Graphics.SimState2MultiCoreStatus
import SoOSiM.Simulator (execStep)
import SoOSiM.Types (SimState)

nextStep :: (MultiCoreStatus, SimState) -> IO (MultiCoreStatus, SimState)
nextStep (mcs,ss) = do
 ns <- execStep ss
 return (updateFromSimState mcs ns,ns)

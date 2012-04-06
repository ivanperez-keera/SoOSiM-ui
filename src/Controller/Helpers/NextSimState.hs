-- | Calculates the new simulation step taking into account changes made to the
-- MultiCoreStatus that reflect user input.
module Controller.Helpers.NextSimState where

-- External imports
import SoOSiM.Simulator (execStep)
import SoOSiM.Types (SimState)

-- Internal imports
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus

-- | Executes one simulation step and updates the multi-core status taking
-- recent changes into account.
nextStep :: (MultiCoreStatus, SimState) -> IO (MultiCoreStatus, SimState)
nextStep (mcs,ss) = do
 ns <- execStep ss
 return (updateFromSimState mcs ns,ns)

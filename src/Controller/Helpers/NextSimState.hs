-- | Calculates the new simulation step taking into account changes made to the
-- MultiCoreStatus that reflect user input.
module Controller.Helpers.NextSimState where

-- External imports
import SoOSiM.Simulator (execStep,execStepSmall)
import SoOSiM.Types (SimState)

-- Internal imports
import Data.History
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
import Model.SystemStatus

-- | Executes one simulation step and updates the multi-core status taking
-- recent changes into account.
nextStep :: (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStep (sys,ss) =
 case future history of
   [] -> nextStep' (sys,ss)
   _  -> return (sys { multiCoreStatus = historyNext history}, ss)
 where history = multiCoreStatus sys

nextStep' :: (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStep' (sys,ss) = do
 let mcs = present history
 ns   <- execStep ss
 mcs' <- updateFromSimState mcs ns
 let sys' = sys { multiCoreStatus = historyBranch history mcs' }
 return (sys',ns)
 where history = multiCoreStatus sys

-- | Executes one simulation step and updates the multi-core status taking
-- recent changes into account.
nextStepSmall :: (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStepSmall (sys,ss) =
 case future history of
   [] -> nextStepSmall' (sys,ss)
   _  -> return (sys { multiCoreStatus = historyNext history}, ss)
 where history = multiCoreStatus sys

nextStepSmall' :: (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStepSmall' (sys,ss) = do
 let mcs = present history
 ns   <- execStepSmall ss
 mcs' <- updateFromSimState mcs ns
 let sys' = sys { multiCoreStatus = historyBranch history mcs' }
 return (sys',ns)
 where history = multiCoreStatus sys

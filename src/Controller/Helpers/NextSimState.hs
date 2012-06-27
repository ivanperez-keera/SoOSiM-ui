-- | Calculates the new simulation step taking into account changes made to the
-- MultiCoreStatus that reflect user input.
module Controller.Helpers.NextSimState where

-- External imports
import Control.Monad
import Hails.MVC.Model.ProtectedModel.Reactive
import SoOSiM.Simulator (execStep,execStepSmall)
import SoOSiM.Types (SimState)

-- Internal imports
import Data.History
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
import Model.SystemStatus

-- Local imports
import CombinedEnvironment
import Model.Model

modelUpdateNextStep :: CEnv -> IO ()
modelUpdateNextStep cenv = do
  st <- getter statusField (model cenv)
  when (isActiveState st) $
    let f = if st == Running then nextStep else nextStepSmall
    in modelUpdateNextStepWith cenv f
 where isActiveState Running     = True
       isActiveState SlowRunning = True
       isActiveState _           = False

modelUpdateNextStepWith :: CEnv -> ((SystemStatus, SimState) -> IO (SystemStatus, SimState)) -> IO ()
modelUpdateNextStepWith cenv nextStepCalc =
  modifierIO simStateField pm $ maybeM $ \state -> do
     (a', b') <- nextStepCalc (simGLSystemStatus state, simGLSimState state)
     return $ state { simGLSystemStatus = a'
                    , simGLSimState     = b'
                    }
 where pm = model cenv

-- FIXME: To be moved to monad extra (or Control.Monad.IfElse)
maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f v = maybe (return Nothing) (liftM Just . f) v

-- | Executes one simulation step and updates the multi-core status taking
-- recent changes into account.
nextStep :: (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStep = nextStepWith nextStep'

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
nextStepSmall = nextStepWith nextStepSmall'

nextStepSmall' :: (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStepSmall' (sys,ss) = do
 let mcs = present history
 ns   <- execStepSmall ss
 mcs' <- updateFromSimState mcs ns
 let sys' = sys { multiCoreStatus = historyBranch history mcs' }
 return (sys',ns)
 where history = multiCoreStatus sys

-- | Executes one simulation step and updates the multi-core status or moves
-- to a future step if available
nextStepWith :: ((SystemStatus, SimState) -> IO (SystemStatus, SimState)) -> (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStepWith f (sys, ss) = 
 case future history of
   [] -> f (sys,ss)
   _  -> return (sys { multiCoreStatus = historyNext history}, ss)
 where history = multiCoreStatus sys

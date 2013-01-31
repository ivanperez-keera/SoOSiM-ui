-- | Calculates the new simulation step taking into account changes made to the
-- MultiCoreStatus that reflect user input.
module Controller.Helpers.NextSimState where

-- External imports
import Control.Monad
import Hails.MVC.Model.ProtectedModel.Reactive
import SoOSiM (tick)
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
    let f = if st == Running then tick else tick
    in modelUpdateNextStepWith cenv f
 where isActiveState Running     = True
       isActiveState SlowRunning = True
       isActiveState _           = False

modelUpdateNextStepWith :: CEnv -> (SimState -> IO SimState) -> IO ()
modelUpdateNextStepWith cenv nextStepCalc =
  modifierIO simStateField (model cenv) $ maybeM $ \state -> do
     (a', b') <- nextStepWith nextStepCalc (simGLSystemStatus state, simGLSimState state)
     return $ state { simGLSystemStatus = a'
                    , simGLSimState     = b'
                    }

-- newSelection :: SystemStatus -> SystemStatus
-- newSelection ss
--   | [] <- selection ss
--   = ss
--   | [nn,cn] <- selection ss
--   , isJust (findRunningElement (nn,cn) (present (multiCoreStatus ss')))
--   | otherwise = ss 

-- FIXME: To be moved to monad extra (or Control.Monad.IfElse)
maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f v = maybe (return Nothing) (liftM Just . f) v

-- | Moves to a future step if available, otherwise executes one simulation
-- step and updates the multi-core status
nextStepWith :: (SimState -> IO SimState) -> (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStepWith f (sys, ss) = 
 case future history of
   [] -> nextStepWith' f (sys,ss)
   _  -> return (sys { multiCoreStatus = historyNext history}, ss)
 where history = multiCoreStatus sys
 
nextStepWith' :: (SimState -> IO SimState) -> (SystemStatus, SimState) -> IO (SystemStatus, SimState)
nextStepWith' f (sys,ss) = do
 let mcs = present history
 ns   <- f ss
 mcs' <- updateFromSimState mcs ns
 let sys' = sys { multiCoreStatus = historyBranch history mcs' }
 return (sys',ns)
 where history = multiCoreStatus sys

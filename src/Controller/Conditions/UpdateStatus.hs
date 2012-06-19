-- | Refreshes the shared simulation state. The new state will be calculated:
--
-- * Regularly, according to the speed selected by the user
--
-- * Every time there's a change in the status (running, paused, stopped, etc).
module Controller.Conditions.UpdateStatus
   ( installHandlers )
  where

-- External imports
import Data.CBMVar
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment
import Controller.Helpers.NextSimState
import Model.Model

-- | Updates the simulation regularly and when there are changes
-- in the simulation status. There's an initial 1-second delay.
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  timeoutAdd (condition cenv) 1000
  model cenv `onEvent` StatusChanged $ void $ condition cenv

-- | Calculates the next step and schedules a new
-- update based on the selected speed
condition :: CEnv -> IO Bool
condition cenv = do
  -- Get status and speed from the model
  st <- getter statusField pm
  sp <- getter speedField pm

  -- If the system is actually running, update the state
  when (st == Running && sp > 0) $
    modifyCBMVar mcsRef $ \(a,b,c,d) -> do (a',b') <- nextStep (a,b)
                                           return (a',b',c,d)

  when (st == SlowRunning && sp > 0) $
    modifyCBMVar mcsRef $ \(a,b,c,d) -> do (a',b') <- nextStepSmall (a,b)
                                           return (a',b',c,d)

  -- If the system is not paused or stopped,
  -- reupdate after a delay
  when (st == Running || st == SlowRunning) $ void $
    let wt = if sp == 0 then 2 else sp
    in timeoutAdd (condition cenv) (round (1000 / wt))

  -- Always stop executing (the timeout handler will
  -- be reinstalled at each step if necessary)
  return False

  where mcsRef = mcs (view cenv)
        pm     = model cenv

-- | Refreshes the shared simulation state. The new state will be calculated:
--
-- * Regularly, according to the speed selected by the user
--
-- * Every time there's a change in the status (running, paused, stopped, etc).
module Controller.Conditions.UpdateStatus
   ( installHandlers )
  where

-- External imports
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

  when (isActiveState st) $ void $ do
    -- If the system is actually running, update the state
    when (sp > 0) $ modelUpdateNextStep cenv
      
      -- let f = if st == Running then nextStep else nextStepSmall
      -- stateM <- getter simStateField pm
      -- awhen stateM $ \state -> do
      --   (a',b') <- f (simGLSystemStatus state, simGLSimState state)
      --   setter simStateField pm $ Just $
      --     state { simGLSystemStatus = a'
      --           , simGLSimState     = b'
      --           }

    -- If the system is not paused or stopped,
    -- schedule an update after a delay
    let wt = if sp == 0 then 2 else sp
    timeoutAdd (condition cenv) (round (1000 / wt))

  -- Always stop executing (the timeout handler will
  -- be reinstalled at each step if necessary)
  return False

  where -- mcsRef = mcs (view cenv)
        pm = model cenv
        isActiveState Running     = True
        isActiveState SlowRunning = True
        isActiveState _           = False

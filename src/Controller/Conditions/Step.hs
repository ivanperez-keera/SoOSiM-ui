-- | Executes one simulation step
module Controller.Conditions.Step
   (installHandlers)
  where

-- External imports
import Control.Monad
import Control.Monad.IfElse
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Internal imports
import CombinedEnvironment
import Controller.Helpers.NextSimState
import Data.History
import Model.Model
import Model.SystemStatus

-- | Executes one simulation step when the user clicks on
-- the Step button
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  stepF <- stepForwardToolBtn ui
  stepF `onToolButtonClicked` conditionF cenv

  stepFS <- stepForwardSmallToolBtn ui
  stepFS `onToolButtonClicked` conditionFS cenv

  stepB <- stepBackToolBtn ui
  stepB `onToolButtonClicked` conditionB cenv
 where ui = view cenv

-- | Updates the state with the next step only if the system is paused
conditionF :: CEnv -> IO()
conditionF cenv = void $ do
  st <- getter statusField pm

  when (st == Paused) $ do
    stateM <- getter simStateField pm
    awhen stateM $ \state -> do
      (a',b') <- nextStep (simGLSystemStatus state, simGLSimState state)
      setter simStateField pm $ Just $
        state { simGLSystemStatus = a'
              , simGLSimState     = b'
              }

  where -- mcsRef = mcs (view cenv)
        pm     = model cenv

-- | Updates the state with the next step only if the system is paused
conditionFS :: CEnv -> IO()
conditionFS cenv = void $ do
  st <- getter statusField pm

  when (st == Paused) $ do
    stateM <- getter simStateField pm
    awhen stateM $ \state -> do
      (a',b') <- nextStepSmall (simGLSystemStatus state, simGLSimState state)
      setter simStateField pm $ Just $
        state { simGLSystemStatus = a'
              , simGLSimState     = b'
              }

  where -- mcsRef = mcs (view cenv)
        pm     = model cenv

conditionB :: CEnv -> IO()
conditionB cenv = void $ do
  st <- getter statusField pm

  when (st == Paused) $ do
    stateM <- getter simStateField pm
    awhen stateM $ \state ->
    -- modifyCBMVar mcsRef $ \state -> 
      setter simStateField pm $ Just $
        state { simGLSystemStatus = previousStatus (simGLSystemStatus state) }

  where -- mcsRef = mcs (view cenv)
        pm     = model cenv

previousStatus :: SystemStatus -> SystemStatus
previousStatus (SystemStatus hist sel) = (SystemStatus hist' sel)
 where hist' = historyBack hist

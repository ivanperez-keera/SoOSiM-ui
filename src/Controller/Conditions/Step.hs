-- | Executes one simulation step
module Controller.Conditions.Step
   (installHandlers)
  where

-- External imports
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive
import SoOSiM.Simulator (execStep,execStepSmall)

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
  st <- getter statusField $ model cenv
  when (st == Paused) $ modelUpdateNextStepWith cenv execStep

-- | Updates the state with the next step only if the system is paused
conditionFS :: CEnv -> IO()
conditionFS cenv = void $ do
  st <- getter statusField $ model cenv
  when (st == Paused) $ modelUpdateNextStepWith cenv execStepSmall

-- | Go back one step
conditionB :: CEnv -> IO()
conditionB cenv = void $ do
  st <- getter statusField pm
  when (st == Paused) $ modifier simStateField pm (fmap previousState)
 where pm = model cenv

previousState :: SimGLState -> SimGLState
previousState state = state { simGLSystemStatus = previousStatus (simGLSystemStatus state) }

previousStatus :: SystemStatus -> SystemStatus
previousStatus (SystemStatus hist sel) = (SystemStatus hist' sel)
 where hist' = historyBack hist

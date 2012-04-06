-- | Executes one simulation step
module Controller.Conditions.Step
   (installHandlers)
  where

-- External imports
import Data.CBMVar
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Internal imports
import CombinedEnvironment
import Controller.Helpers.NextSimState
import Model.Model

-- | Executes one simulation step when the user clicks on
-- the Step button
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  step <- stepForwardToolBtn ui
  step `onToolButtonClicked` condition cenv
 where ui = uiBuilder $ view cenv

-- | Updates the state with the next step only if the system is paused
condition :: CEnv -> IO()
condition cenv = void $ do
  st <- getter statusField pm

  when (st == Paused) $
    modifyCBMVar mcsRef nextStep

  where mcsRef = mcs (view cenv)
        pm     = model cenv

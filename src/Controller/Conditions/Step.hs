module Controller.Conditions.Step where

--- import Data.CBRef
import Data.CBMVar
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment
import Graphics.MultiCoreStatus
import Model.Model
import SoOSiM.Simulator (execStep)
import SoOSiM.Types (SimState)

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  step <- stepForwardToolBtn ui
  step `onToolButtonClicked` condition cenv
 where ui = uiBuilder $ view cenv

condition :: CEnv -> IO()
condition cenv = void $ do
  st <- getter statusField pm

  when (st == Paused) $
    modifyCBMVar mcsRef nextStep

  where mcsRef = mcs (view cenv)
        pm     = model cenv

nextStep :: (MultiCoreStatus,Maybe SimState) -> IO (MultiCoreStatus, Maybe SimState)
nextStep (x,y) = do
 ns <- maybe (return Nothing) (fmap Just . execStep) y
 -- return (toggleStatus ("PU1", "P2") x, ns)
 return (x,ns)

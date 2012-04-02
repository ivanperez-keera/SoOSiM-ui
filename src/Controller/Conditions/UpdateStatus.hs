module Controller.Conditions.UpdateStatus where

--- import Data.CBRef
import Data.CBMVar
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment
import Graphics.MultiCoreStatus
import Model.Model

-- For now, this simply changes the state of a component every two seconds
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  timeoutAdd (toggleSt cenv) 1000
  model cenv `onEvent` StatusChanged $ void $ toggleSt cenv

toggleSt :: CEnv -> IO Bool
toggleSt cenv = do
  st <- getter statusField pm
  sp <- getter speedField pm

  when (st == Running && sp > 0) $ do
    modifyCBMVar mcsRef (return.nextStep)

  when (st == Running) $ void $
    let wt = if sp == 0 then 2 else sp
    in timeoutAdd (toggleSt cenv) (round (1000 / wt))

  return False

  where mcsRef = mcs (view cenv)
        pm     = model cenv

nextStep :: MultiCoreStatus -> MultiCoreStatus
nextStep = toggleStatus ("PU1", "P2")

-- | Refreshes the shared simulation state. The new state will be calculated:
--
-- * Regularly, according to the speed selected by the user
--
-- * Every time there's a change in the status (running, paused, stopped, etc).
module Controller.Conditions.ShowState
   ( installHandlers )
  where

-- External imports
import Control.Monad
import Control.Monad.IfElse
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import Data.History
import CombinedEnvironment
import Graphics.UI.Gtk.Display.SoOSiMState
import Model.Model
import Model.SystemStatus

-- | Updates the simulation regularly and when there are changes
-- in the simulation status. There's an initial 1-second delay.
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
  onEvent (model cenv) SimStateChanged $ condition cenv

condition :: CEnv -> IO ()
condition cenv = onViewAsync $ do
  stateM <- getter simStateField (model cenv)
  awhen stateM $ \state -> do
    let systemSt = simGLSystemStatus state
        simstate = simGLSimState state
        sel      = selection $ systemSt
        mcs      = present $ multiCoreStatus systemSt
    soosimSetSimState soosim $ Just simstate
    soosimSetMCS soosim $ Just mcs
    soosimSetSelection soosim $ Just sel
    where soosim = soosimView (view cenv)

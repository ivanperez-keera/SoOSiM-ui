-- | Adjusts the speed of the simulation to the user selection.
-- Speed can be controlled using two kinds of elements:
--
--  * The speed up/down, pause, stop, run buttons
--
--  * A speed slider on the status bar
--
-- All of them must be synchronised with the internal
-- speed and among them (changes to the speed using
-- the buttons must be reflected both in the animation
-- and the slider)
--
module Controller.Conditions.Speed
   (installHandlers)
  where

-- External imports
import Control.Monad
import Graphics.UI.Gtk
import Hails.Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ProtectedModel.Reactive

-- Internal imports
import CombinedEnvironment
import Data.History
import Graphics.Diagrams.MultiCoreStatus
import Model.Model
import Model.SystemStatus
import SoOSiM.Samples.Initializer

-- | Adjusts the system speed and running/paused status
-- to the user selection
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let vw = view cenv

  run <- runToolBtn vw
  run `onToolButtonClicked` conditionRun cenv

  slowRun <- runSlowToolBtn vw
  slowRun `onToolButtonClicked` conditionSlowRun cenv

  pause <- pauseToolBtn vw
  pause `onToolButtonClicked` conditionPause cenv

  stop <- stopToolBtn vw
  stop `onToolButtonClicked` conditionStop cenv

  speedUp <- speedUpToolBtn vw
  speedUp `onToolButtonClicked` conditionSpeedUp cenv

  slowDown <- slowDownToolBtn vw
  slowDown `onToolButtonClicked` conditionSlowDown cenv

  -- Handle the speed slider in the status bar
  hscale <- fmap reactiveScale $ speedScale vw
  installCondition cenv (hscale =:= speedField)
  
-- | Sets the system as running
conditionRun :: CEnv -> IO()
conditionRun cenv = setter statusField (model cenv) Running

conditionSlowRun :: CEnv -> IO ()
conditionSlowRun cenv = setter statusField (model cenv) SlowRunning

-- | If the system is running or paused, it toggles the state
conditionPause :: CEnv -> IO()
conditionPause cenv = modifier statusField (model cenv) togglePauseSt
 where togglePauseSt Running     = Paused
       togglePauseSt SlowRunning = Paused
       togglePauseSt Paused      = Running
       togglePauseSt x           = x

-- | Halts and restarts the simulation
conditionStop :: CEnv -> IO()
conditionStop cenv = do
  setter statusField pm Stopped

  -- Starts a fresh new simulation
  ss <- simstate
  let emptySystemStatus = SystemStatus (historyNew emptyMultiCoreStatus) []
      mcs'              = SimGLState emptySystemStatus ss []
  setter simStateField pm (Just mcs')

  setter statusField  pm Paused
 where pm = model cenv

-- | Increases the simulation speed by a factor of 2
conditionSpeedUp :: CEnv -> IO()
conditionSpeedUp cenv = modifier speedField (model cenv) (*2)

-- | Decreases the simulation speed by a factor of 2, down to a minimum of 0.1
conditionSlowDown :: CEnv -> IO()
conditionSlowDown cenv = modifier speedField (model cenv) slowDown
 where slowDown curSpeed = if curSpeed >= 0.2 then curSpeed / 2 else curSpeed

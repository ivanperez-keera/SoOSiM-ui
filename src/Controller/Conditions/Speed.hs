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
import Data.CBMVar
import GHC.Float
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive
import Hails.MVC.Controller.ConditionDirection

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
      pm = model cenv

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
  hscale <- speedScale vw
  hscale `on` valueChanged $ conditionSpeedChanged VM cenv
  onEvent pm SpeedChanged $ conditionSpeedChanged MV cenv
  onEvent pm Initialised  $ conditionSpeedChanged MV cenv

-- | Sets the system as running
conditionRun :: CEnv -> IO()
conditionRun cenv = setter statusField pm Running
 where pm = model cenv

conditionSlowRun :: CEnv -> IO ()
conditionSlowRun cenv =
  setter statusField pm SlowRunning
 where pm = model cenv

-- | If the system is running or paused, it toggles the state
conditionPause :: CEnv -> IO()
conditionPause cenv = do
  st <- getter statusField pm
  let st' = pauseSt st
  when (st /= st') $ setter statusField pm st'
 where pm = model cenv
       pauseSt Running     = Paused
       pauseSt SlowRunning = Paused
       pauseSt Paused      = Running
       pauseSt x           = x

-- | Halts and restarts the simulation
conditionStop :: CEnv -> IO()
conditionStop cenv = do
  setter statusField pm Stopped

  -- Starts a fresh new simulation
  ss <- simstate
  let emptySystemStatus = SystemStatus (historyNew emptyMultiCoreStatus) []
      mcs'              = SimGLState emptySystemStatus ss []
  modifyCBMVar mcsRef $ \_ -> return mcs'

  setter statusField pm Paused
 where pm     = model cenv
       mcsRef = mcs $ view cenv

-- | Increases the simulation speed by a factor of 2
conditionSpeedUp :: CEnv -> IO()
conditionSpeedUp cenv = do
  curSp <- getter speedField pm
  setter speedField pm (curSp * 2)
 where pm = model cenv

-- | Decreases the simulation speed by a factor of 2
conditionSlowDown :: CEnv -> IO()
conditionSlowDown cenv = do
  curSp <- getter speedField pm
  when (curSp >= 0.2) $ setter speedField pm (curSp / 2)
 where pm = model cenv

-- | Maintains the coherence between the simulation's speed and the slider's
--
-- FIXME: To be implemented using (=:=)
conditionSpeedChanged :: ConditionDirection -> CEnv -> IO()
conditionSpeedChanged cd cenv = do
  -- View value
  hscale <- speedScale vw

  -- Model value
  curSp <- getter speedField pm
  curV  <- fmap double2Float $ get hscale rangeValue

  -- Update in the appropriate direction when values differ
  when (curV /= curSp) $
    case cd of
     MV -> set hscale [ rangeValue := float2Double curSp ]
     VM -> setter speedField pm curV

 where pm = model cenv
       vw = view cenv

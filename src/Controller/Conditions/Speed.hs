module Controller.Conditions.Speed where

import Control.Monad
import GHC.Float
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive
import Hails.MVC.Controller.ConditionDirection

import CombinedEnvironment
import Model.Model

-- For now, this simply changes the state of a component every two seconds
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let ui = uiBuilder $ view cenv
      pm = model cenv

  pause <- pauseToolBtn ui
  pause `onToolButtonClicked` conditionPause cenv

  run <- runToolBtn ui
  run `onToolButtonClicked` conditionRun cenv

  stop <- stopToolBtn ui
  stop `onToolButtonClicked` conditionStop cenv

  speedUp <- speedUpToolBtn ui
  speedUp `onToolButtonClicked` conditionSpeedUp cenv

  slowDown <- slowDownToolBtn ui
  slowDown `onToolButtonClicked` conditionSlowDown cenv

  hscale <- hscale1 ui
  hscale `on` valueChanged $ conditionSpeedChanged VM cenv
  onEvent pm SpeedChanged $ conditionSpeedChanged MV cenv
  onEvent pm Initialised $ conditionSpeedChanged MV cenv

conditionPause :: CEnv -> IO()
conditionPause cenv = do
  st <- getter statusField pm
  case st of
   Running -> setter statusField pm Paused
   Paused  -> setter statusField pm Running
   Stopped -> return ()
 where pm = model cenv

conditionRun :: CEnv -> IO()
conditionRun cenv =
  setter statusField pm Running
 where pm = model cenv

conditionStop :: CEnv -> IO()
conditionStop cenv =
  setter statusField pm Stopped
 where pm = model cenv

conditionSpeedUp :: CEnv -> IO()
conditionSpeedUp cenv = do
  curSp <- getter speedField pm
  setter speedField pm (curSp * 2)
 where pm = model cenv

conditionSlowDown :: CEnv -> IO()
conditionSlowDown cenv = do 
  curSp <- getter speedField pm
  when (curSp >= 0.2) $ setter speedField pm (curSp / 2)
 where pm = model cenv

-- To be implemented using (=:=)
conditionSpeedChanged :: ConditionDirection -> CEnv -> IO()
conditionSpeedChanged cd cenv = do
  -- View value
  hscale <- hscale1 ui

  -- Model value
  curSp <- getter speedField pm
  curV  <- fmap double2Float $ get hscale rangeValue 

  when (curV /= curSp) $
    case cd of
     MV -> set hscale [ rangeValue := float2Double curSp ]
     VM -> setter speedField pm curV

 where pm = model cenv
       ui = uiBuilder $ view cenv

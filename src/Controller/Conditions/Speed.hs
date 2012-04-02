module Controller.Conditions.Speed where

import Data.CBRef
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment
import Graphics.MultiCoreStatus
import Model.Model

-- For now, this simply changes the state of a component every two seconds
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let ui = uiBuilder $ view cenv
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

conditionPause :: CEnv -> IO()
conditionPause cenv = do
  st <- getter statusField pm
  case st of
   Running -> setter statusField pm Paused
   Paused  -> setter statusField pm Running
   Stopped -> return ()
 where pm = model cenv

conditionRun :: CEnv -> IO()
conditionRun cenv = do
  let pm = model cenv
  setter speedField  pm (1.0 :: Float)
  setter statusField pm Running

conditionStop :: CEnv -> IO()
conditionStop cenv = do
  let pm = model cenv
  setter speedField  pm (0.0 :: Float)
  setter statusField pm Stopped

conditionSpeedUp :: CEnv -> IO()
conditionSpeedUp cenv = do
  let pm = model cenv
  curSt <- getter statusField pm
  curSp <- getter speedField pm
  if curSp == 0.0
    then setter speedField pm (0.1 :: Float)
    else setter speedField pm (curSp * 2)

conditionSlowDown :: CEnv -> IO()
conditionSlowDown cenv = do 
  let pm = model cenv
  curSt <- getter statusField pm
  curSp <- getter speedField pm
  when (curSp >= 0.2) $
    setter speedField pm (curSp / 2)

module Controller.Conditions.Fullscreen where

--- import Data.CBRef
import Data.CBMVar
import Control.Monad
import Graphics.UI.Gtk
import Control.Monad.Trans
import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment
import Graphics.MultiCoreStatus
-- import Model.Model

-- For now, this simply changes the state of a component every two seconds
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  w <- window1 $ uiBuilder $ view cenv
  w `on` windowStateEvent $ do e <- eventWindowState
                               liftIO $ condition (WindowStateFullscreen `elem` e) cenv
                               return True

  fmi <- fullScreenMenuItem $ uiBuilder $ view cenv
  fmi `on` menuItemActivate $ conditionToggle cenv

  fmb <- fullScreenToolBtn $ uiBuilder $ view cenv
  fmb `onToolButtonClicked` conditionToggle cenv

condition :: Bool -> CEnv -> IO()
condition winfs cenv = do
  mfs <- getter fullscreenField pm
  when (mfs /= winfs) $
    setter fullscreenField pm winfs
 where pm = model cenv

conditionToggle :: CEnv -> IO()
conditionToggle cenv = do
  mfs    <- getter fullscreenField pm
  window <- window1 $ uiBuilder $ view cenv
  if mfs
   then windowUnfullscreen window
   else windowFullscreen window
 where pm = model cenv

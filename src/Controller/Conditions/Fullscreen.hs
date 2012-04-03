module Controller.Conditions.Fullscreen where

-- External imports
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

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
  let toggle = if mfs then windowUnfullscreen else windowFullscreen
  toggle window
 where pm = model cenv

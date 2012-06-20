-- | Condition: the UI will toggle the fullscreen mode when the user clicks on
-- the fullscreen icon or the fullscreen menu item.
module Controller.Conditions.Fullscreen
  (installHandlers)
 where

-- External imports
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

-- | Fullscreen mode handling:
--
-- * Updates the fullscreen mode in the model when the window's fullscreen
-- status changes.
--
-- * Toggles the window's fullscreen mode when the user clicks on the FS
-- menu item or the toolbar icon.
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  w <- mainWindow $ view cenv

  -- Save the new status in the model when it changes
  w `on` windowStateEvent $ do
    fs <- fmap (WindowStateFullscreen `elem`) eventWindowState
    liftIO $ conditionUpdateModel fs cenv -- (WindowStateFullscreen `elem` e) cenv
    return True

  -- Toggle fullscreen mode
  fmi <- fullScreenMenuItem $ view cenv
  fmi `on` menuItemActivate $ conditionToggle cenv
  fmb <- fullScreenToolBtn $ view cenv
  fmb `onToolButtonClicked` conditionToggle cenv

-- | Save the new window status in the model
conditionUpdateModel :: Bool -> CEnv -> IO()
conditionUpdateModel winfs cenv = do
  mfs <- getter fullscreenField pm
  when (mfs /= winfs) $
    setter fullscreenField pm winfs
 where pm = model cenv

-- | Toggles the window's fullscreen status
conditionToggle :: CEnv -> IO()
conditionToggle cenv = do
  mfs    <- getter fullscreenField pm
  window <- mainWindow $ view cenv
  let toggle = if mfs then windowUnfullscreen else windowFullscreen
  toggle window
 where pm = model cenv

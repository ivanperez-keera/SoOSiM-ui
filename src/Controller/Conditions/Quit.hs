-- | Closes the application when the user closes the main window
-- or clicks on Quit in the File menu
module Controller.Conditions.Quit
   (installHandlers)
  where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment

-- | Closes the application when the user closes the main window
-- or clicks on Quit in the File menu
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  window <- window1 $ uiBuilder $ view cenv
  onDestroy window quit

  mn <- quitMenuItem $ uiBuilder $ view cenv
  mn `on` menuItemActivate $ quit

-- | Quits the application
quit :: IO ()
quit = mainQuit

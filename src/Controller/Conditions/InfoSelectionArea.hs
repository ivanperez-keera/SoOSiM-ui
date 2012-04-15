-- | Condition: the notebook showing the component info will show the page
-- corresponding to the icon selected in the iconview (info, trace)
module Controller.Conditions.InfoSelectionArea
  (installHandlers)
 where

-- External imports
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment

-- | Selects the appropriate page in the info notebook as the user
-- chooses between basic info and component trace
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  iv <- iconview1 $ uiBuilder $ view cenv
  iv `on` selectionChanged $ condition cenv
  model cenv `onEvent` Initialised $ condition cenv

-- | Shows appropriate page based on current selection
condition :: CEnv -> IO()
condition cenv = do
  iv        <- iconview1 $ uiBuilder $ view cenv
  nb        <- notebook3 $ uiBuilder $ view cenv
  (path, _) <- iconViewGetCursor iv
  case path of
   [x] -> notebookSetCurrentPage nb (x + 1)
   _   -> notebookSetCurrentPage nb 0

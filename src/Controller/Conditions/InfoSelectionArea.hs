-- | Condition: the notebook showing the component info will show the page
-- corresponding to the icon selected in the iconview (info, trace)
module Controller.Conditions.InfoSelectionArea
  (installHandlers)
 where

-- External imports
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.ModelViewNotebookSync

-- Local imports
import CombinedEnvironment

-- | Selects the appropriate page in the info notebook as the user
-- chooses between basic info and component trace
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  iv <- infoIconView $ view cenv
  iv `on` selectionChanged $ condition cenv
  model cenv `onEvent` Initialised $ condition cenv

-- | Shows appropriate page based on current selection
condition :: CEnv -> IO()
condition cenv = do

  -- Get widgets
  iv <- infoIconView ui
  nb <- infoSelNotebook ui

  -- Calculate page number and show it
  let pageF = maybe 0 (+1) . listToMaybe
  modelViewNotebookSync iv nb pageF

 where ui = view cenv

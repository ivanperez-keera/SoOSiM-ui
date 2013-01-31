-- | Reacts to changes in the selection in the diagram
module Controller.Conditions.Selection
   (installHandlers)
  where

-- External imports
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Internal imports
import CombinedEnvironment
import Model.Model
import Model.SystemStatus
import View
import Graphics.UI.Gtk.Display.SoOSiMState

-- | Handles changes in the box selection in the diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  onEvent (model cenv) SimStateChanged $ conditionShowPage cenv
  soosimOnSelectionChanged soosim $ conditionUpdateSelection cenv
 where vw = view cenv
       soosim = soosimView vw
  
-- | Shows component info only when a component is selected
conditionShowPage :: CEnv -> IO()
conditionShowPage cenv = do
  stM <- getter simStateField (model cenv) -- readCBMVar $ mcs $ view cenv
  let hasSelection = maybe False (not . null . selection . simGLSystemStatus) stM

  notebook <- infoNotebook vw

  -- Show notebook page
  let newPage = if hasSelection then 1 else 0
  notebookSetCurrentPage notebook newPage
 where vw = view cenv
       soosim = soosimView vw

-- | Shows component info only when a component is selected
conditionUpdateSelection :: CEnv -> IO()
conditionUpdateSelection cenv = do
  selection <- soosimGetSelection soosim
  stM <- getter simStateField (model cenv)
  when (isJust stM) $ do
    let selection' = fromMaybe [] selection
        stM'       = fromJust stM
        status     = simGLSystemStatus stM'
        status'    = status { selection = selection' }
        stM''      = stM' { simGLSystemStatus = status' }
    setter simStateField (model cenv) (Just stM'')

 where vw = view cenv
       soosim = soosimView vw


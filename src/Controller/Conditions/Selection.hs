-- | Reacts to changes in the selection in the diagram
module Controller.Conditions.Selection
   (installHandlers)
  where

-- External imports
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive

-- Internal imports
import CombinedEnvironment
import Model.Model
import Model.SystemStatus

-- | Handles changes in the box selection in the diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
  onEvent (model cenv) SimStateChanged $ conditionShowPage cenv
  
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

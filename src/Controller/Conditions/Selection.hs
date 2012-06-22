-- | Reacts to changes in the selection in the diagram
module Controller.Conditions.Selection
   (installHandlers)
  where

-- External imports
import Control.Monad
import Data.CBMVar
import Graphics.UI.Gtk

-- Internal imports
import CombinedEnvironment
import Model.SystemStatus

-- | Handles changes in the box selection in the diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
  installCallbackCBMVar mcsRef $ conditionShowPage cenv
 where mcsRef = mcs (view cenv)
  
-- | Shows component info only when a component is selected
conditionShowPage :: CEnv -> IO()
conditionShowPage cenv = do
  hasSelection <- fmap (not . null . selection . simGLSystemStatus) $ readCBMVar $ mcs vw
  notebook     <- infoNotebook vw

  -- Show notebook page
  let newPage = if hasSelection then 1 else 0
  notebookSetCurrentPage notebook newPage
 where vw = view cenv

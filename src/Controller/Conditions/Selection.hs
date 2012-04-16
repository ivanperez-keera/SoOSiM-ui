-- | Reacts to changes in the selection in the gloss diagram
module Controller.Conditions.Selection
   (installHandlers)
  where

-- External imports
import Control.Monad
import Data.CBMVar
import Data.Tuple4
import Graphics.UI.Gtk

-- Internal imports
import CombinedEnvironment
import Model.SystemStatus

-- | Handles changes in the box selection in the gloss diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
   installCallbackCBMVar mcsRef $ conditionShowPage cenv
  where mcsRef = mcs (view cenv)
  
-- | Shows component info only when a component is selected
conditionShowPage :: CEnv -> IO()
conditionShowPage cenv = do
 st <- readCBMVar $ mcs $ view cenv
 nb <- notebook1 $ uiBuilder $ view cenv
 let sel = selection $ fst4 st
 case sel of
  [] -> notebookSetCurrentPage nb 0
  _  -> notebookSetCurrentPage nb 1

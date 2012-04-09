-- | Reacts to changes in the selection in the gloss diagram
module Controller.Conditions.Selection
   (installHandlers)
  where

-- External imports
import Control.Monad
import Data.CBMVar

-- Internal imports
import CombinedEnvironment
import Graphics.Diagrams.MultiCoreStatus

-- | Handles changes in the box selection in the gloss diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
   installCallbackCBMVar mcsRef $ condition cenv
  where mcsRef = mcs (view cenv)
  
-- | Prints the current selection to stdout
condition :: CEnv -> IO()
condition =
 print . selection . fst3 <=< readCBMVar . mcs . view
 where fst3 (a,b,c) = a

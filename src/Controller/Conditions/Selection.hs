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
condition cenv = do
 st <- readCBMVar $ mcs $ view cenv
 print $ fth4 st
 where fst4 (a,b,c,d) = a
       fth4 (a,b,c,d) = d

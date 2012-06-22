-- | Reacts to changes in the selection in the diagram
module Controller.Conditions.InfoTooltip
   (installHandlers)
  where

-- External imports
import Control.Monad
import Data.CBMVar
import Data.Maybe
-- <<<<<<< HEAD
-- import Data.Tuple4
-- =======
-- >>>>>>> b9d5608c07bc9f8f4ea2328106ce4548ac199fa9
import Graphics.UI.Gtk

-- Internal imports
import CombinedEnvironment
import Graphics.Diagrams.Types
import Data.History
import Model.SystemStatus
import Graphics.Diagrams.MultiCoreStatus

-- | Handles changes in the box selection in the diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
   installCallbackCBMVar mcsRef $ conditionShowInfo cenv
  where mcsRef = mcs (view cenv)
  
-- | Shows component info only when a component is selected
conditionShowInfo :: CEnv -> IO()
conditionShowInfo cenv = do
  -- Get elem info if possible
  st <- readCBMVar $ mcs $ view cenv
  let tt = getElemInfo (simGLSelection st) $ 
             present $ multiCoreStatus $ simGLSystemStatus st

  -- Update label text
  lbl <- statusLbl $ view cenv
  labelSetText lbl $ fromMaybe "" tt

-- | Renders the info relative to a given element (both basic info and a trace)
getElemInfo :: [Name]          -- ^ The qualified name of the element whose info we need
            -> MultiCoreStatus -- ^ The current SimState
            -> Maybe String    -- ^ If the element exists, it's associated info, and
                               --   Nothing otherwise
getElemInfo [x,y] ss = getCompInfo x y ss
getElemInfo _     _  = Nothing

-- | Compiles the component info
getCompInfo :: Name -> Name -> MultiCoreStatus -> Maybe String
getCompInfo nn cn ss = do
  re <- findRunningElement (nn, cn) ss
  return $ showCompStatus nn cn re

-- | Renders a small string with the summarised component status
showCompStatus :: Name -> Name -> RunningElement -> String
showCompStatus _nn cn re = concat
   [ show cn
   , " : " ++ cKind
   , " | " ++ st
   , " | Cycle count: " ++ show (compCyclesRunning stats) ++ "(R)"
     ++ "/" ++ show (compCyclesWaiting stats) ++ "(W)"
     ++ "/" ++ show (compCyclesIdling stats) ++ "(I)"
   ]
 where stats   = elementStatistics re
       cKind   = elementKind re
       cStatus = elementState re

       st = case cStatus of
              Idle    -> "Idle"
              Active  -> "Running"
              Waiting -> "Waiting"

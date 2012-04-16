-- | Reacts to changes in the selection in the gloss diagram
module Controller.Conditions.InfoTooltip
   (installHandlers)
  where

-- External imports
import           Control.Concurrent.STM
import           Control.Monad
import           Data.CBMVar
import qualified Data.IntMap            as I
import           Data.List
import           Data.Maybe
import           Data.Tuple4
import           Graphics.UI.Gtk
import qualified SoOSiM.Types           as S

-- Internal imports
import CombinedEnvironment
import Graphics.Diagrams.Types
import Data.History
import Model.SystemStatus
import Graphics.Diagrams.MultiCoreStatus

-- | Handles changes in the box selection in the gloss diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
   installCallbackCBMVar mcsRef $ conditionShowPage cenv
  where mcsRef = mcs (view cenv)
  
-- | Shows component info only when a component is selected
conditionShowPage :: CEnv -> IO()
conditionShowPage cenv = do
 -- Get elem info if possible
 st <- readCBMVar $ mcs $ view cenv
 let tt = getElemInfo (fth4 st) $ present $ multiCoreStatus $ fst4 st

 -- Update label text
 lbl <- statusLbl $ uiBuilder $ view cenv
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
  return $ showCompInfo nn cn re

-- | Renders two strings with the basic component info and the trace
showCompInfo :: Name -> Name -> RunningElement -> String
showCompInfo nn cn re = 
  map (\x -> if x == '\n' then ' ' else x) $ unlines
   [ show cn
   , ": "   ++ cKind
   , "| " ++ st
   , "| Cycle count: " ++ show (compCyclesRunning stats) ++ "(R)"
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

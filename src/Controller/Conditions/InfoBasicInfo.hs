-- | Condition: the notebook showing the component info will show basic info of
-- the currently selected component
module Controller.Conditions.InfoBasicInfo
  (installHandlers)
 where

-- External imports
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IfElse
import           Data.CBMVar
import qualified Data.IntMap            as I
import           Data.List
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Helpers.Multiline.TextBufferHelpers
import qualified SoOSiM.Types           as S

-- Local imports
import CombinedEnvironment
import Data.Tuple4
import Data.History
import Graphics.Diagrams.MultiCoreStatus
import Model.SystemStatus
import Graphics.Diagrams.Types

-- | Selects the appropriate page in the info notebook as the user
-- chooses between basic info and component trace
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
   installCallbackCBMVar mcsRef $ conditionShowCompInfo cenv
  where mcsRef = mcs (view cenv)
  
-- | Shows component info of the selected component
conditionShowCompInfo :: CEnv -> IO()
conditionShowCompInfo cenv = do
 st  <- readCBMVar $ mcs $ view cenv
 bf1 <- textViewGetBuffer <=< infoTextView  $ uiBuilder $ view cenv
 bf2 <- textViewGetBuffer <=< traceTextView $ uiBuilder $ view cenv

 let sel   = selection $ fst4 st
     mcs   = present $ multiCoreStatus $ fst4 st
     -- simSt = snd4 st

 awhen (getElemInfo sel mcs) $ \(ni,nt) -> do
   textBufferUpdateText bf1 ni
   textBufferUpdateText bf2 nt

-- | Renders the info relative to a given element (both basic info and a trace)
getElemInfo :: [Name]                 -- ^ The qualified name of the element whose info we need
            -> MultiCoreStatus        -- ^ The current SimState
            -> Maybe (String, String) -- ^ If the element exists, it's associated info, and
                                      --   Nothing otherwise
getElemInfo [x]   ss = getNodeInfo x ss
getElemInfo [x,y] ss = getCompInfo x y ss
getElemInfo _     _  = Nothing

-- | Gets the node info 
getNodeInfo :: Name -> MultiCoreStatus -> Maybe (String, String)
getNodeInfo n _ss =
   Just (binfo, tinfo)
  where binfo = "Node: " ++ n
        tinfo = "Traces are only available for components"

-- | Compiles the component info
getCompInfo :: Name -> Name -> MultiCoreStatus -> Maybe (String, String)
getCompInfo nn cn ss = do
  re <- findRunningElement (nn, cn) ss
  return $ showCompInfo nn cn re

-- | Renders two strings with the basic component info and the trace
showCompInfo :: Name -> Name -> RunningElement -> (String, String)
showCompInfo nn cn cc =
  let bi  = showCompBasicInfo nn cn cc
      msg = showCompTrace nn cn cc
  in (bi, msg)

-- | Creates a string with the basic component info for a given S.ComponentContext
showCompBasicInfo :: Name -> Name -> RunningElement -> String
showCompBasicInfo nn cn re = unlines
  [ "Component pid: "    ++ cn
  , "Component kind: "   ++ cKind
  , "Node: "             ++ nn
  , "Component status: " ++ st
  , "Cycles running: "   ++ show (compCyclesRunning stats)
  , "Cycles waiting: "   ++ show (compCyclesWaiting stats)
  , "Cycles idling: "    ++ show (compCyclesIdling stats)
  ]
 where stats   = elementStatistics re
       cKind   = elementKind re
       cStatus = elementState re

       st = case cStatus of
              Idle    -> "Idle"
              Active  -> "Running"
              Waiting -> "Waiting"

-- | Creates a component trace from a given S.ComponentContext
showCompTrace :: Name -> Name -> RunningElement -> String
showCompTrace _nn _cn = unlines . compTrace . elementStatistics

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
import qualified SoOSiM.Types           as S

-- Local imports
import CombinedEnvironment
import Data.Tuple4
import Graphics.Diagrams.MultiCoreStatus
import Model.SystemStatus
import Graphics.Diagrams.Types
import Graphics.UI.Gtk.Multiline.TextBufferHelpers

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

 let sel   = selection (fst4 st)
     simSt = snd4 st

 awhenM (getElemInfo sel simSt) $ \(ni,nt) -> do
   textBufferUpdateText bf1 ni
   textBufferUpdateText bf2 nt

-- | Renders the info relative to a given element (both basic info and a trace)
getElemInfo :: [Name]                      -- ^ The qualified name of the element whose info we need
            -> S.SimState                  -- ^ The current SimState
            -> IO (Maybe (String, String)) -- ^ If the element exists, it's associated info, and
                                           --   Nothing otherwise
getElemInfo [x]   ss = getNodeInfo x ss
getElemInfo [x,y] ss = getCompInfo x y ss
getElemInfo _     _  = return Nothing

-- | Gets the node info 
getNodeInfo :: Name -> S.SimState -> IO (Maybe (String, String))
getNodeInfo n _ss = return $ do
   -- (_,e) <- find ((n ==) . show . S.nodeId . snd) ns
   let binfo = "Node: " ++ n
       tinfo = "Traces are only available for components"
   return (binfo, tinfo)
  -- where ns = I.toList $ S.nodes ss

-- | Compiles the component info
getCompInfo :: Name -> Name -> S.SimState -> IO (Maybe (String, String))
getCompInfo nn cn ss =
  maybe (return Nothing) (fmap Just . showCompInfo nn cn ss) ctx
  where ctx = getCompCtx nn cn ss

-- | Gets the component context for a given node+name combination
getCompCtx :: Name -> Name -> S.SimState -> Maybe S.ComponentContext
getCompCtx nn cn ss = do

   -- Find node by name
   (_,e) <- find ((nn ==) . show . S.nodeId . snd) ns

   -- Find component in node by name
   (_,c) <- find ((cn ==) . show . S.componentId . snd) $ I.toList $ S.nodeComponents e

   -- Return component
   return c
  where ns = I.toList $ S.nodes ss

-- | Renders two strings with the basic component info and the trace
showCompInfo :: Name -> Name -> S.SimState -> S.ComponentContext -> IO (String, String)
showCompInfo nn cn ss cc = do
  bi  <- showCompBasicInfo nn cn ss cc
  msg <- showCompTrace nn cn ss cc
  return (bi, msg)

-- | Creates a string with the basic component info for a given S.ComponentContext
showCompBasicInfo :: Name -> Name -> S.SimState -> S.ComponentContext -> IO String
showCompBasicInfo nn cn _ss (S.CC _cid csu cse _cr _buf _trc smd) = do
    metaData <- readTVarIO smd
    cKind    <- fmap S.componentName $ readTVarIO cse
    cStatus  <- readTVarIO csu

    let st = case cStatus of
              S.Idle                -> "Idle"
              S.Running             -> "Running"
              S.WaitingForMsg sid _ -> "Waiting for message from " ++ show sid

    return $ unlines
     [ "Component pid: "    ++ cn
     , "Component kind: "   ++ cKind
     , "Node: "             ++ nn
     , "Component status: " ++ st
     , "Cycles running: "   ++ show (S.cyclesRunning metaData)
     , "Cycles waiting: "   ++ show (S.cyclesWaiting metaData)
     , "Cycles idling: "    ++ show (S.cyclesIdling metaData)
     ]

-- | Creates a component trace from a given S.ComponentContext
showCompTrace :: Name -> Name -> S.SimState -> S.ComponentContext -> IO String
showCompTrace _nn _cn _ss (S.CC _ _ _ _ _ trc _) = return $ unlines trc

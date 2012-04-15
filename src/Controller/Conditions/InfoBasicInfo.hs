-- | Condition: the notebook showing the component info will show basic info of
-- the currently selected component
module Controller.Conditions.InfoBasicInfo
  (installHandlers, getElemInfo)
 where

-- External imports
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.CBMVar
import Data.List
import Graphics.UI.Gtk
import Hails.MVC.Model.ProtectedModel.Reactive
import qualified Data.IntMap as I
import qualified SoOSiM.Types as S
import           Unique

-- Local imports
import CombinedEnvironment
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types

-- | Selects the appropriate page in the info notebook as the user
-- chooses between basic info and component trace
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
   installCallbackCBMVar mcsRef $ conditionShowCompInfo cenv
  where mcsRef = mcs (view cenv)
  
-- | Shows component info of the selected component
conditionShowCompInfo :: CEnv -> IO()
conditionShowCompInfo cenv = do
 st  <- readCBMVar $ mcs $ view cenv
 bf1 <- textViewGetBuffer <=< textview1 $ uiBuilder $ view cenv
 bf2 <- textViewGetBuffer <=< textview2 $ uiBuilder $ view cenv

 ei  <- getElemInfo (selection (fst4 st)) (snd4 st)
 case ei of
  Nothing      -> return ()
  Just (ni,nt) -> do textBufferSetText bf1 ni
                     textBufferSetText bf2 nt
 where fst4 (a,_,_,_) = a
       snd4 (_,b,_,_) = b

-- | Updates a text buffer only if it's necessary (to avoid extra events)
textBufferUpdateText :: TextBuffer -> String -> IO ()
textBufferUpdateText bf s = do
 tx <- textBufferGetAllText bf
 when (tx /= s) $ textBufferSetText bf s

-- | Gets all the text from a text buffer
textBufferGetAllText :: TextBuffer -> IO String
textBufferGetAllText bf = do
 si <- textBufferGetStartIter bf
 ei <- textBufferGetEndIter bf
 textBufferGetText bf si ei True

getElemInfo :: [Name] -> S.SimState -> IO (Maybe (String, String))
getElemInfo [x]   ss = getNodeInfo x ss
getElemInfo [x,y] ss = getCompInfo x y ss
getElemInfo _     _  = return Nothing

-- | Gets the node info 
getNodeInfo :: Name -> S.SimState -> IO (Maybe (String, String))
getNodeInfo n ss = return $ do
   (_,e) <- find ((n ==) . show . S.nodeId . snd) ns
   let binfo = unlines ["Node: " ++ show (S.nodeId e)]
   return (binfo, "Traces are only available for components")
  where ns = I.toList $ S.nodes ss

-- | Compiles the component info
getCompInfo :: Name -> Name -> S.SimState -> IO (Maybe (String, String))
getCompInfo nn cn ss = do
  let ctx = getCompCtx nn cn ss
  case ctx of
   Nothing   -> return Nothing
   Just ctx' -> fmap Just $ showCompInfo nn cn ss ctx'

-- | Gets the component context for a given node+name combination
getCompCtx :: Name -> Name -> S.SimState -> Maybe S.ComponentContext
getCompCtx nn cn ss = do
   (_,e) <- find ((nn ==) . show . S.nodeId . snd) ns
   (_,c) <- find ((cn ==) . show . S.componentId . snd) $ I.toList $ S.nodeComponents e
   return c
  where ns = I.toList $ S.nodes ss

-- | Renders two strings with the basic component info and the trace
showCompInfo :: Name -> Name -> S.SimState -> S.ComponentContext -> IO (String, String)
showCompInfo nn cn ss cc@(S.CC cid csu cse cr buf trc smd) = do
  bi  <- showCompBasicInfo nn cn ss cc
  msg <- showCompTrace nn cn ss cc
  return (bi, msg)

-- | Creates a string with the basic component info for a given S.ComponentContext
showCompBasicInfo :: Name -> Name -> S.SimState -> S.ComponentContext -> IO String
showCompBasicInfo nn cn ss (S.CC cid csu cse cr buf trc smd) = do
    metaData <- readTVarIO smd
    cKind    <- fmap S.componentName $ readTVarIO cse
    cStatus  <- readTVarIO csu

    let st = case cStatus of
              S.Idle                -> "Idle"
              S.Running             -> "Running"
              S.WaitingForMsg sid _ -> "Waiting for message from " ++ show sid

    return $ unlines
     [ "Component pid: "    ++ show cid
     , "Component kind: "   ++ cKind
     , "Component status: " ++ st
     , "Cycles running: "   ++ show (S.cyclesRunning metaData)
     , "Cycles waiting: "   ++ show (S.cyclesWaiting metaData)
     , "Cycles idling: "    ++ show (S.cyclesIdling metaData)
     ]

-- | Creates a component trace from a given S.ComponentContext
showCompTrace :: Name -> Name -> S.SimState -> S.ComponentContext -> IO String
showCompTrace nn cn ss (S.CC cid csu cse cr buf trc smd) =
  return $ unlines trc

-- | Reacts to changes in the selection in the gloss diagram
module Controller.Conditions.InfoTooltip
   (installHandlers)
  where

-- External imports
import Control.Concurrent.STM
import Control.Monad
import Data.CBMVar
import Data.List
import Data.Maybe
import Graphics.UI.Gtk
import qualified Data.IntMap  as I
import qualified SoOSiM.Types as S
import           Unique

-- Internal imports
import CombinedEnvironment
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types

-- | Handles changes in the box selection in the gloss diagram
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
   installCallbackCBMVar mcsRef $ conditionShowPage cenv
  where mcsRef = mcs (view cenv)
  
-- | Shows component info only when a component is selected
conditionShowPage :: CEnv -> IO()
conditionShowPage cenv = do
 st  <- readCBMVar $ mcs $ view cenv
 let sel = fth4 st
 tt  <- getElemInfo sel (snd4 st)
 lbl <- label12 $ uiBuilder $ view cenv

 let txt = map (\x -> if x == '\n' then ' ' else x) $ fromMaybe "" tt
 labelSetText lbl txt
 
 where fth4 (a,b,c,d) = d
       snd4 (a,b,c,d) = b

getElemInfo :: [Name] -> S.SimState -> IO (Maybe String)
getElemInfo [x,y] ss = getCompInfo x y ss
getElemInfo _     _  = return Nothing

-- | Compiles the component info
getCompInfo :: Name -> Name -> S.SimState -> IO (Maybe String)
getCompInfo nn cn ss = do
  let ctx = getCompCtx nn cn ss
  case ctx of
   Nothing   -> return Nothing
   Just ctx' -> fmap Just $ showCompBasicInfo nn cn ss ctx'

-- | Gets the component context for a given node+name combination
getCompCtx :: Name -> Name -> S.SimState -> Maybe S.ComponentContext
getCompCtx nn cn ss = do
   (_,e) <- find ((nn ==) . show . S.nodeId . snd) ns
   (_,c) <- find ((cn ==) . show . S.componentId . snd) $ I.toList $ S.nodeComponents e
   return c
  where ns = I.toList $ S.nodes ss

-- | Creates a string with the basic component info for a given S.ComponentContext
showCompBasicInfo :: Name -> Name -> S.SimState -> S.ComponentContext -> IO String
showCompBasicInfo nn cn ss (S.CC cid csu cse cr buf trc smd) = do
    metaData <- readTVarIO smd
    cKind    <- fmap S.componentName $ readTVarIO cse
    cStatus  <- readTVarIO csu

    let st = case cStatus of
              S.Idle              -> "Idle"
              S.Running           -> "Running"
              S.WaitingForMsg _ _ -> "Waiting"

    return $ unlines
     [ show cid
     , ": "   ++ cKind
     , "| " ++ st
     , "| Cycle count: " ++ show (S.cyclesRunning metaData) ++ "(R)"
       ++ "/" ++ show (S.cyclesWaiting metaData) ++ "(W)"
       ++ "/" ++ show (S.cyclesIdling metaData) ++ "(I)"
     ]

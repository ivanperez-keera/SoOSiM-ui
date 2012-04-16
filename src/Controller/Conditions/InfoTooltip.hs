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
 tt <- getElemInfo (fth4 st) (snd4 st)

 -- Update label text
 lbl <- statusLbl $ uiBuilder $ view cenv
 labelSetText lbl $ fromMaybe "" tt

-- | Gets the summarised info from components only
getElemInfo :: [Name] -> S.SimState -> IO (Maybe String)
getElemInfo [x,y] ss = getCompInfo x y ss
getElemInfo _     _  = return Nothing

-- | Compiles the component info
getCompInfo :: Name -> Name -> S.SimState -> IO (Maybe String)
getCompInfo nn cn ss =
  case ctx of
   Nothing   -> return Nothing
   Just ctx' -> fmap Just $ showCompBasicInfo nn cn ss ctx'
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

    return $ map (\x -> if x == '\n' then ' ' else x) $ unlines
     [ show cid
     , ": "   ++ cKind
     , "| " ++ st
     , "| Cycle count: " ++ show (S.cyclesRunning metaData) ++ "(R)"
       ++ "/" ++ show (S.cyclesWaiting metaData) ++ "(W)"
       ++ "/" ++ show (S.cyclesIdling metaData) ++ "(I)"
     ]

-- | Condition: the notebook showing the component info will show basic info of
-- the currently selected component
module Controller.Conditions.InfoBasicInfo
  (installHandlers)
 where

-- External imports
import Control.Monad
import Control.Monad.IfElse
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.Multiline.TextBuffer
import Hails.MVC.Model.ProtectedModel.Reactive

-- Local imports
import CombinedEnvironment
import Data.History
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types
import Model.Model
import Model.SystemStatus

-- | Selects the appropriate page in the info notebook as the user
-- chooses between basic info and component trace
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
  onEvent (model cenv) SimStateChanged $ conditionShowCompInfo cenv
  
-- | Shows component info of the selected component
conditionShowCompInfo :: CEnv -> IO()
conditionShowCompInfo cenv = onViewAsync $ do
 stM  <- getter simStateField (model cenv) -- readCBMVar $ mcs $ view cenv
 awhen stM $ \st -> do
   -- bf1 <- textViewGetBuffer <=< infoTextView  $ view cenv
   -- bf2 <- textViewGetBuffer <=< traceTextView $ view cenv

   let sel   = selection $ simGLSystemStatus st
       mcs   = present $ multiCoreStatus $ simGLSystemStatus st

   case (sel, getElemInfo sel mcs) of
     ([], _) -> do textViewUpdateText cenv infoTextView "" 
                   textViewUpdateText cenv traceTextView ""
     (_, Nothing) -> do textViewUpdateText cenv infoTextView  "The selected component has ceased to exist" 
                        textViewUpdateText cenv traceTextView ""
     (_, Just (ni,nt)) -> do textViewUpdateText cenv infoTextView  ni
                             textViewUpdateText cenv traceTextView nt

-- | Auxiliary function that updates a text view. We need to use this function
-- instead of calling textBufferUpdateText because buffers cannot be reused.
-- textViewUpdateText :: (TextViewClass b, GtkGUI a)
--                    => CEnv -> (a -> IO b) -> String -> IO ()
textViewUpdateText cenv f text = do
  bf <- textViewGetBuffer <=< f $ view cenv
  textBufferUpdateText bf text

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
getNodeInfo n _ss = Just (binfo, tinfo)
  where binfo = "Node: " ++ n
        tinfo = "Traces are only available for components"

-- | Compiles the component info
getCompInfo :: Name -> Name -> MultiCoreStatus -> Maybe (String, String)
getCompInfo nn cn ss = do
  re <- findRunningElement (nn, cn) ss
  return $ showCompInfo nn cn re

-- | Renders two strings with the basic component info and the trace
showCompInfo :: Name -> Name -> RunningElement -> (String, String)
showCompInfo nn cn cc = (bi, msg)
 where bi  = showCompBasicInfo nn cn cc
       msg = showCompTrace nn cn cc

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

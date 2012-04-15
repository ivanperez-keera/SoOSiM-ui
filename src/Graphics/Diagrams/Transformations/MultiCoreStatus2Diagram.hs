-- | Transforms MultiCoreStatus' into diagrams
module Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram
   ( transformStatus )
  where

-- External imports
import Data.Maybe

-- Internal imports
import Config.Preferences
import Config.Config
import Graphics.Diagrams.Simple.Diagram
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types (Name)

-- | Transform a multicore status into a diagram
transformStatus :: Config -> MultiCoreStatus -> Diagram
transformStatus cfg (MultiCoreStatus ps ms s) = Diagram ps' ms'
 where ps' = mapMaybe (transformProcessingUnit cfg s) ps
       ms' = map transformMessage ms

-- | Transforms a processing unit into a box
transformProcessingUnit :: Config -> [Name] -> ProcessingUnit -> Maybe Box
transformProcessingUnit _   _   (ProcessingUnit _ _  UnitIgnored) = Nothing
transformProcessingUnit cfg sel (ProcessingUnit n cs e) =
   Just $ GroupBox n cs' col expand
 where cs'    = if expand then map (transformRunningElement cfg sel') cs else []
       col    = processingUnitColor cfg (sel == [n])
       sel'   = if not (null sel) && head sel == n then tail sel else []
       expand = e == UnitExpanded

-- | Transforms a running element (component, application) into a box
transformRunningElement :: Config -> [Name] -> RunningElement -> Box
transformRunningElement cfg ns (Component n k s _)   = Box n k (runningElementColor cfg (ns == [n]) s)
transformRunningElement cfg ns (Application n k s _) = Box n k (runningElementColor cfg (ns == [n]) s)

-- | Transforms a message into an arrow
transformMessage :: Message -> Arrow
transformMessage m = Arrow (sender m) (receiver m)

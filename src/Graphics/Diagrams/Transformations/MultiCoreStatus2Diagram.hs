-- | Transforms MultiCoreStatus' into diagrams
module Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram
   ( transformStatus )
  where

-- External imports
import Data.Maybe

-- Internal imports
import Config.Preferences
import Graphics.Diagrams.Simple.Diagram
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types (Name)

-- | Transform a multicore status into a diagram
transformStatus :: MultiCoreStatus -> Diagram
transformStatus (MultiCoreStatus ps ms s) = Diagram ps' ms'
 where ps' = mapMaybe (transformProcessingUnit s) ps
       ms' = map transformMessage ms

-- | Transforms a processing unit into a box
transformProcessingUnit :: [Name] -> ProcessingUnit -> Maybe Box
transformProcessingUnit _   (ProcessingUnit _ _  UnitIgnored) = Nothing
transformProcessingUnit sel (ProcessingUnit n cs e) =
   Just $ GroupBox n cs' col expand
 where cs'    = if expand then map (transformRunningElement sel') cs else []
       col    = processingUnitColor (sel == [n])
       sel'   = if not (null sel) && head sel == n then tail sel else []
       expand = e == UnitExpanded

-- | Transforms a running element (component, application) into a box
transformRunningElement :: [Name] -> RunningElement -> Box
transformRunningElement ns (Component n k s _)   = Box n k (runningElementColor (ns == [n]) s)
transformRunningElement ns (Application n k s _) = Box n k (runningElementColor (ns == [n]) s)

-- | Transforms a message into an arrow
transformMessage :: Message -> Arrow
transformMessage m = Arrow (sender m) (receiver m)

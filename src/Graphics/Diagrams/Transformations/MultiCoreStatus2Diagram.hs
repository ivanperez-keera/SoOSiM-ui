-- | Transforms MultiCoreStatus' into diagrams
module Graphics.Diagrams.Transformations.MultiCoreStatus2Diagram
   ( transformStatus )
  where

-- External imports
import Data.Maybe

-- Internal imports
import Graphics.Diagrams.Simple.Diagram
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Types (Color, makeColor, Name)

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
       col    = puStatusToColor (sel == [n])
       sel'   = if not (null sel) && head sel == n then tail sel else []
       expand = e == UnitExpanded

-- | Transforms a running element (component, application) into a box
transformRunningElement :: [Name] -> RunningElement -> Box
transformRunningElement ns (Component n k s _)   = Box n k (reStatusToColor (ns == [n]) s)
transformRunningElement ns (Application n k s _) = Box n k (reStatusToColor (ns == [n]) s)

-- | Transforms a message into an arrow
transformMessage :: Message -> Arrow
transformMessage m = Arrow (sender m) (receiver m)

-- | Transforms the current processing unit status (selected) into a colour
puStatusToColor :: Bool -> Color
puStatusToColor True  = makeColor 0.1 0.5 0.9 1.0
puStatusToColor False = makeColor 0.9 0.9 0.9 1.0

-- | Transforms the current running element status (Selected, State) into a
-- colour
reStatusToColor :: Bool -> ElementState -> Color
reStatusToColor True  Active  = makeColor 0.5 0.9 0.7 1.0
reStatusToColor True  Waiting = makeColor 0.9 0.9 0.3 1.0
reStatusToColor True  Idle    = makeColor 0.9 0.9 0.6 1.0
reStatusToColor False Active  = makeColor 0.5 0.9 0.9 1.0
reStatusToColor False Waiting = makeColor 0.9 0.9 0.5 1.0
reStatusToColor False Idle    = makeColor 0.9 0.9 0.9 1.0

-- | Transforms MultiCoreStatus' into diagrams
module Graphics.MultiCoreStatus2Diagram
   ( transformStatus
   , transformProcessingUnit
   , transformRunningElement
   , transformMessage
   )
  where

-- External imports
import Data.Maybe

-- Internal imports
import Graphics.Diagram
import Graphics.MultiCoreStatus
import Graphics.Types (Color, makeColor, Name)

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
transformRunningElement ns (Component _ n s _)   = Box n (reStatusToColor (ns == [n]) s)
transformRunningElement ns (Application _ n s _) = Box n (reStatusToColor (ns == [n]) s)

-- | Transforms a message into an arrow
transformMessage :: Message -> Arrow
transformMessage m = Arrow (qNames (sender m)) (qNames (receiver m))
  where qNames (n1, n2) = [n1, n2]

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

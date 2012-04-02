{-# LANGUAGE PackageImports #-}
module Graphics.MultiCoreStatus2Diagram where

import Data.Maybe

import Graphics.MultiCoreStatus
import Graphics.Diagram
import Graphics.Types (Color, makeColor, Name)

transformStatus :: MultiCoreStatus -> Diagram
transformStatus (MultiCoreStatus ps ms s) = Diagram ps' ms'
 where ps' = mapMaybe (transformProcessingUnit s) ps
       ms' = map transformMessage ms

transformProcessingUnit :: [Name] -> ProcessingUnit -> Maybe Box
transformProcessingUnit sel (ProcessingUnit n cs UnitExpanded) =
   Just $ GroupBox n cs' col
 where cs' = map (transformRunningElement sel') cs
       col = if sel == [n]
              then makeColor 0.1 0.5 0.9 1.0
              else makeColor 0.9 0.9 0.9 1.0
       sel' = if not (null sel) && head sel == n then tail sel else []

transformProcessingUnit sel (ProcessingUnit n cs UnitCollapsed) =
   Just $ Box n col
  where col = if sel == [n]
                then makeColor 0.1 0.5 0.9 1.0
                else makeColor 0.9 0.9 0.9 1.0
transformProcessingUnit _ (ProcessingUnit n cs UnitIgnored) = Nothing

transformRunningElement :: [Name] -> RunningElement -> Box
transformRunningElement ns (Component _ n s _)   = Box n (statusToColor (ns == [n]) s)
transformRunningElement ns (Application _ n s _) = Box n (statusToColor (ns == [n]) s)

transformMessage :: Message -> Arrow
transformMessage m = Arrow (qNames (sender m)) (qNames (receiver m))
  where qNames (n1, n2) = [n1, n2]

statusToColor :: Bool -> ElementState -> Color
statusToColor True  Active  = makeColor 0.5 0.9 0.7 1.0
statusToColor True  Waiting = makeColor 0.9 0.9 0.3 1.0
statusToColor True  Idle    = makeColor 0.9 0.9 0.6 1.0
statusToColor False Active  = makeColor 0.5 0.9 0.9 1.0
statusToColor False Waiting = makeColor 0.9 0.9 0.5 1.0
statusToColor False Idle    = makeColor 0.9 0.9 0.9 1.0

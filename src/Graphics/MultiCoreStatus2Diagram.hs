{-# LANGUAGE PackageImports #-}
module Graphics.MultiCoreStatus2Diagram where

import Data.Maybe

import Graphics.MultiCoreStatus
import Graphics.Diagram
import Graphics.Types (Color, makeColor)

transformStatus :: MultiCoreStatus -> Diagram
transformStatus (MultiCoreStatus ps ms) = Diagram ps' ms'
 where ps' = mapMaybe transformProcessingUnit ps
       ms' = map transformMessage ms

transformProcessingUnit :: ProcessingUnit -> Maybe Box
transformProcessingUnit (ProcessingUnit n cs UnitExpanded) =
   Just $ GroupBox n cs' $ makeColor  0.9 0.9 0.9 1.0
 where cs' = map transformRunningElement cs
transformProcessingUnit (ProcessingUnit n cs UnitCollapsed) =
   Just $ Box n $ makeColor  0.9 0.9 0.9 1.0
transformProcessingUnit (ProcessingUnit n cs UnitIgnored) =
   Nothing

transformRunningElement :: RunningElement -> Box
transformRunningElement (Component _ n s _)   = Box n (statusToColor s)
transformRunningElement (Application _ n s _) = Box n (statusToColor s)

transformMessage :: Message -> Arrow
transformMessage m = Arrow (qNames (sender m)) (qNames (receiver m))
  where qNames (n1, n2) = [n1, n2]

statusToColor :: ElementState -> Color
statusToColor Active  = makeColor 0.5 0.9 0.9 1.0
statusToColor Waiting = makeColor 0.9 0.9 0.5 1.0
statusToColor Idle    = makeColor 0.9 0.9 0.9 1.0

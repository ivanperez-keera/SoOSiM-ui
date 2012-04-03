{-# LANGUAGE PackageImports #-}
module Graphics.SimState2MultiCoreStatus where

import           Data.Maybe
import qualified Data.IntMap as I
import qualified Data.Map    as M
import qualified SoOSiM.Types as S

import Graphics.MultiCoreStatus
import Graphics.Diagram
import Graphics.Types (Color, makeColor, Name)

updateFromSimState :: MultiCoreStatus -> S.SimState -> MultiCoreStatus
updateFromSimState mcs ss = 
  MultiCoreStatus ps ms s
 where ps = map (updateNode mcs) ns
       ms = concatMap (collectMessages mcs) ns
       ns = I.toList $ S.nodes ss
       s  = selection mcs

updateNode :: MultiCoreStatus -> (Int, S.Node) -> ProcessingUnit
updateNode mcs (i,n) =
 maybe u g (findProcessingUnit (show (S.nodeId n)) mcs)
 where u = node2ProcessingUnit mcs (i, n)
       g (ProcessingUnit _ _ e) = u { unitStatus = e }

node2ProcessingUnit :: MultiCoreStatus -> (Int, S.Node) -> ProcessingUnit
node2ProcessingUnit mcs (i, n) = ProcessingUnit name cs UnitExpanded
 where name = show (S.nodeId n)
       cs   = map (component2RunningElement mcs) $ I.toList $ S.nodeComponents n

component2RunningElement :: MultiCoreStatus -> (Int, S.ComponentContext) -> RunningElement
component2RunningElement mcs (i, c) = Component pid name state Nothing
 where pid   = show i
       name  = compStateName c
       state = compStateState c

compStateName :: S.ComponentContext -> String
compStateName (S.CC _ s _ _) = S.componentName s

compStateState :: S.ComponentContext -> ElementState
compStateState (S.CC s _ _ _) =
  case s of
    S.Running           -> Active
    S.WaitingForMsg _ _ -> Waiting
    S.Idle              -> Idle

collectMessages :: MultiCoreStatus -> (Int, S.Node) -> [Message]
collectMessages _ _ = []

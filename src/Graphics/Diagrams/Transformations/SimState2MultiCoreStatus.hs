-- | Updates a multicore status from a 'SimState' keeping
-- the selection.
module Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
   (updateFromSimState)
  where

import qualified Data.IntMap as I
import qualified SoOSiM.Types as S

import Graphics.Diagrams.MultiCoreStatus

-- | Updates a multicore status from a 'SimState' maintaining the
-- previous selection.
updateFromSimState :: MultiCoreStatus -> S.SimState -> MultiCoreStatus
updateFromSimState mcs ss = 
  MultiCoreStatus ps ms s
 where ps = map (updateNode mcs) ns
       ms = concatMap (collectMessages mcs) ns
       ns = I.toList $ S.nodes ss
       s  = selection mcs

-- | Updates a node in a MultiCore System from a SoOSiM node
updateNode :: MultiCoreStatus -> (Int, S.Node) -> ProcessingUnit
updateNode mcs (i,n) =
 maybe u g (findProcessingUnit (show (S.nodeId n)) mcs)
 where u = node2ProcessingUnit mcs (i, n)
       g (ProcessingUnit _ _ e) = u { unitStatus = e }

-- | Transforms a SoOSiM node into a Processing Unit description
node2ProcessingUnit :: MultiCoreStatus -> (Int, S.Node) -> ProcessingUnit
node2ProcessingUnit mcs (i, n) = ProcessingUnit name cs UnitExpanded
 where name = show (S.nodeId n)
       cs   = map (component2RunningElement mcs) $ I.toList $ S.nodeComponents n

-- | Transforms a SoOSiM component into a running element
component2RunningElement :: MultiCoreStatus -> (Int, S.ComponentContext) -> RunningElement
component2RunningElement mcs (i, c) = Component pid name state Nothing
 where pid   = show i
       name  = compStateName c
       state = compStateState c

-- | Obtains the component name from its context
compStateName :: S.ComponentContext -> String
compStateName (S.CC _ s _ _) = S.componentName s

-- | Obtains the component state from its context
compStateState :: S.ComponentContext -> ElementState
compStateState (S.CC s _ _ _) =
  case s of
    S.Running           -> Active
    S.WaitingForMsg _ _ -> Waiting
    S.Idle              -> Idle

-- | Transforms the SoOSiM messages into MultiCore description messages
collectMessages :: MultiCoreStatus -> (Int, S.Node) -> [Message]
collectMessages _ _ = []

-- | Updates a multicore status from a 'SimState' keeping
-- the selection.
module Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
   (updateFromSimState)
  where

import Control.Concurrent.STM
import qualified Data.IntMap as I
import qualified SoOSiM.Types as S

import Graphics.Diagrams.MultiCoreStatus

-- | Updates a multicore status from a 'SimState' maintaining the
-- previous selection.
updateFromSimState :: MultiCoreStatus -> S.SimState -> IO MultiCoreStatus
updateFromSimState mcs ss = do
   let ns = I.toList $ S.nodes ss
       s  = selection mcs
   ms <- mapM (collectMessages mcs) ns
   ps <- mapM (updateNode mcs) ns
   return $ MultiCoreStatus ps (concat ms) s
 -- where ps = map (updateNode mcs) ns
 --       ms = concatMap (collectMessages mcs) ns

-- | Updates a node in a MultiCore System from a SoOSiM node
updateNode :: MultiCoreStatus -> (Int, S.Node) -> IO ProcessingUnit
updateNode mcs (i,n) =
 maybe u g (findProcessingUnit (show (S.nodeId n)) mcs)
 where u = node2ProcessingUnit mcs (i, n)
       g (ProcessingUnit _ _ e) = fmap (\x -> x { unitStatus = e }) u

-- | Transforms a SoOSiM node into a Processing Unit description
node2ProcessingUnit :: MultiCoreStatus -> (Int, S.Node) -> IO ProcessingUnit
node2ProcessingUnit mcs (i, n) = do
  cs <- mapM (component2RunningElement mcs) $ I.toList $ S.nodeComponents n
  return $ ProcessingUnit name cs UnitExpanded
 where name = show (S.nodeId n)

-- | Transforms a SoOSiM component into a running element
component2RunningElement :: MultiCoreStatus -> (Int, S.ComponentContext) -> IO RunningElement
component2RunningElement mcs (i, c) = do
  name  <- compStateName c
  state <- compStateState c
  return $ Component pid name state Nothing
 where pid = show i

-- | Obtains the component name from its context
compStateName :: S.ComponentContext -> IO String
compStateName (S.CC _ _ s _ _) = do
  fmap S.componentName $ readTVarIO s

-- | Obtains the component state from its context
compStateState :: S.ComponentContext -> IO ElementState
compStateState (S.CC _ s _ _ _) = do
  s' <- readTVarIO s
  case s' of
    S.Running           -> return Active
    S.WaitingForMsg _ _ -> return Waiting
    S.Idle              -> return Idle

-- | Transforms the SoOSiM messages into MultiCore description messages
collectMessages :: MultiCoreStatus -> (Int, S.Node) -> IO [Message]
collectMessages _ _ = return []

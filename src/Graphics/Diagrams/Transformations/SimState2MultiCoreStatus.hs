{-# LANGUAGE PatternGuards #-}
-- | Updates a multicore status from a 'SimState' keeping
-- the selection.
module Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
   (updateFromSimState)
  where

-- External imports
import           Control.Concurrent.STM
import           Data.Maybe             (listToMaybe)
import qualified Data.IntMap            as I
import qualified SoOSiM.Types           as S
import           Unique

-- Internal imports
import Graphics.Diagrams.MultiCoreStatus

-- | Updates a multicore status from a 'SimState' maintaining the
-- previous selection.
updateFromSimState :: MultiCoreStatus -> S.SimState -> IO MultiCoreStatus
updateFromSimState mcs ss = do
   let ns = I.toList $ S.nodes ss
       s  = selection mcs
   ms <- mapM (collectMessages ns) ns
   ps <- mapM (updateNode mcs) ns
   return $ MultiCoreStatus ps (concat ms) s

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
 where pid = show (getUnique i)

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
collectMessages :: [(Int, S.Node)] -> (Int, S.Node) -> IO [Message]
collectMessages nodes (nid,n) = do
  msgs <- mapM (collectMessagesCC nodes nid) $ I.toList $ S.nodeComponents n
  return $ concat msgs

-- | Gets the list of messages from a given node and component
collectMessagesCC :: [(Int, S.Node)] -> Int -> (Int, S.ComponentContext) -> IO [Message]
collectMessagesCC nodes nid (cid, cc) = do
  inputs <- compPendingInputs cc
  msgs   <- mapM (collectMessagesInput nodes nid cid) inputs
  return $ concat msgs

-- | Transforms an input SoOSiM message into a MCS message
collectMessagesInput :: [(Int, S.Node)] -> Int -> Int -> S.ComponentInput -> IO [Message]
collectMessagesInput nodes nid cid (S.ComponentMsg sid _)
 | Just x <- findComponentNode sid nodes
 = return $ [ Message [show x, show sid] [show (getUnique nid), show (getUnique cid)] "" ]
collectMessagesInput nodes nid cid (S.NodeMsg sid _) =
  return [ Message [show sid] [show (getUnique nid), show (getUnique cid)] "" ]
collectMessagesInput _ _ _ _ = return []

-- | Gets the list of pending inputs from a component context
compPendingInputs :: S.ComponentContext -> IO [S.ComponentInput]
compPendingInputs (S.CC _ _ _ _ b) = readTVarIO b

-- | Returns the node id of the node that the given component is running in,
-- if any.
findComponentNode :: S.ComponentId -> [(Int, S.Node)] -> Maybe S.NodeId
findComponentNode cid ns = listToMaybe
  [ S.nodeId n | (_,n) <- ns, I.member (getKey cid) (S.nodeComponents n) ]
-- findComponentNode cid [] = Nothing
-- findComponentNode cid ((i,n):ns)
--  | I.member (getKey cid) (S.nodeComponents n) = Just (S.nodeId n)
--  | otherwise                                  = findComponentNode cid ns

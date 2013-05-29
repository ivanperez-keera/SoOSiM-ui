{-# LANGUAGE PatternGuards #-}
-- | Updates a multicore status from a 'SimState' keeping
-- the selection.
module Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
   (updateFromSimState)
  where

-- External imports
import           Control.Concurrent.STM
import           Data.Dynamic
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
   ms <- concatMapM (collectMessages ns) ns
   ps <- mapM (updateNode mcs) ns
   return $ MultiCoreStatus ps ms

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
  stats <- compStatistics c
  return $ Component pid name state Nothing stats
 where pid = show (getKey (getUnique i))

-- | Obtains the component name from its context
compStateName :: S.ComponentContext -> IO String
compStateName (S.CC s _ _ _ _ _ _ _) =
  return (S.componentName s)

-- | Obtains the component state from its context
compStateState :: S.ComponentContext -> IO ElementState
compStateState (S.CC _ _ _ s _ mb _ _) = do
  s'  <- readTVarIO s
  mb' <- readTVarIO mb
  return $ case (s', mb') of
            (S.Running _ _   , _)  -> Active
            (S.WaitingFor _ _, _)  -> Waiting
            (S.ReadyToIdle   , []) -> Idle
            (S.ReadyToRun    , []) -> Idle
            (S.ReadyToIdle   , _)  -> Active
            (S.ReadyToRun    , _)  -> Active

compStatistics :: S.ComponentContext -> IO Statistics
compStatistics (S.CC _s _cid _csu cse _cr _buf trc smd) = do
    metaData <- readTVarIO smd
    return $ Statistics (S.cyclesRunning metaData)
                        (S.cyclesWaiting metaData)
                        (S.cyclesIdling metaData)
                        (map fst trc)


-- | Transforms the SoOSiM messages into MultiCore description messages
collectMessages :: [(Int, S.Node)] -> (Int, S.Node) -> IO [Message]
collectMessages nodes (nid,n) = concatMapM getMessages nodeComponentsL
 where nodeComponentsL = I.toList $ S.nodeComponents n
       getMessages     = collectMessagesCC nodes nid

-- | Gets the list of messages from a given node and component
collectMessagesCC :: [(Int, S.Node)] -> Int -> (Int, S.ComponentContext) -> IO [Message]
collectMessagesCC nodes nid (cid, cc) =
   concatMapM getMessagesInput =<< compPendingInputs cc
  where getMessagesInput = collectMessagesInput nodes nid cid

-- | Transforms an input SoOSiM message into a MCS message
collectMessagesInput :: [(Int, S.Node)] -> Int -> Int -> S.Input a -> IO [Message]
collectMessagesInput nodes nid cid input
 | (S.Message _ _ sid) <- input
 , Just senderNode <- findComponentNode (fst (S.unRA sid)) nodes
 = return [ Message [show senderNode, show (fst (S.unRA sid))] dest "" ]

 -- | (S.NodeMsg sid _) <- input
 -- = return [ Message [show sid] dest "" ]

 | otherwise
 = return []

 where dest = map (show . getKey . getUnique) [nid, cid]

-- | Gets the list of pending inputs from a component context
compPendingInputs :: S.ComponentContext -> IO [S.Input Dynamic]
compPendingInputs (S.CC _ _ _ _ _ b _ _) = readTVarIO b

-- | Returns the node id of the node that the given component is running in,
-- if any.
findComponentNode :: S.ComponentId -> [(Int, S.Node)] -> Maybe S.NodeId
findComponentNode cid ns = listToMaybe
  [ S.nodeId n | (_, n) <- ns, I.member cid (S.nodeComponents n) ]

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

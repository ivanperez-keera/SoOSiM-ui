module SoOSiM.Components.ProcessManager where

import Data.Maybe

import SoOSiM
import SoOSiM.Components.ApplicationHandler.Types
import SoOSiM.Components.CodeDeployer.Types
import SoOSiM.Components.ProcessManager.Types
import SoOSiM.Components.ProcessManager.Util
import SoOSiM.Components.ResourceDiscovery.Types
import SoOSiM.Components.Types.Code
import SoOSiM.Components.Types.Node

instance ComponentIface ProcessManagerState where
 initState          = ProcessManagerState Nothing []
 componentName _    = "ProcessManager"
 componentBehaviour state msg = do
   -- Find the components that we need and defer the operation
   appHndlrIdM     <- componentLookup Nothing "ApplicationHandler"
   resMgrIdM       <- componentLookup Nothing "ResourceDiscovery"
   codeDeployerIdM <- componentLookup Nothing "CodeDeployer"
   let argsM = do appHndlrId     <- appHndlrIdM
                  resMgrId       <- resMgrIdM
                  codeDeployerId <- codeDeployerIdM
                  return (SchedulerDependencyIds appHndlrId resMgrId codeDeployerId)
   maybe' argsM
     (return state)
     (componentBehaviourWithIds state msg)

componentBehaviourWithIds ::
  ProcessManagerState
  -> ComponentInput
  -> SchedulerDependencyIds
  -> SimM ProcessManagerState
componentBehaviourWithIds state msg deps
    -- Execute File
    | ComponentMsg sender content <- msg
    , Just (Execute f)       <- fromDynamic content
    = do
      -- Obtain list of block names, codes and necessary resources
      rsp <- fmap fromDynamic $ invoke Nothing (appHndlr deps) (toDyn (AppHndlrMsg f))
      -- If a list has been obtained, execute the first block
      maybe' rsp
        (return state)
        (\app -> let state' = state {appDataM = Just app}
                 in  startThreads state' 1 (head $ appBlocks app) sender deps)

    -- Create n threads running the block blockName (if it exists)
    | ComponentMsg sender content <- msg
    , Just (CreateThreads numThreads blockName) <- fromDynamic content
    , Just (b,r) <- lookupBlock state blockName
    = startThreads state numThreads (blockName, b, r) sender deps

    --
    | otherwise = yield state
  where
    lookupBlock ::
      ProcessManagerState
      -> BlockName
      -> Maybe (BlockCode, ResourceReq)
    lookupBlock st bName = do
      (AppData xs) <- appDataM st
      listToMaybe [ (c, r) | (b,c,r) <- xs, b == bName]


-- Starts several threads and stores the new Ids in the state
startThreads ::
  ProcessManagerState
  -> Int
  -> (BlockName, BlockCode, ResourceReq)
  -> ComponentId
  -> SchedulerDependencyIds
  -> SimM ProcessManagerState
startThreads state numThreads (bname, b, r) callerId deps = do
  nodesM <- fmap fromDynamic $ invoke Nothing (resMgr deps) (toDyn (RequestResource True numThreads r))

  -- If we get a list of nodes, deploy the same code in each one of them
  maybe' nodesM (return state) $ \(FoundNodes nodes) -> do
    -- deploy the threads and get the component ids
    newIds <- mapM (\node -> deployAndCompute state
                               bname b node callerId deps)
                               nodes
    -- update the scheduler state
    yield $ state { runningIds = catMaybes newIds ++ runningIds state }


-- | Deploys code in one node and requests that it is computed
deployAndCompute ::
  ProcessManagerState
  -> String
  -> BlockCode
  -> NodeDef
  -> ComponentId
  -> SchedulerDependencyIds
  -> SimM (Maybe ComponentId)
deployAndCompute state name block node callerId deps = do
  -- Request that the block is deployed and obtain component id
  compIdM <- fmap fromDynamic $ invoke Nothing (codeDeployer deps) (toDyn (DeployBlock name block node callerId))

  -- Send compute to the newly created component
  maybe' compIdM (return ()) $ \compId ->
    invokeNoWait Nothing compId (toDyn Compute)

  return compIdM

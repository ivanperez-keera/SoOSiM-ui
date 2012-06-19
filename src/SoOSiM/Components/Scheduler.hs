{-# LANGUAGE ScopedTypeVariables #-}
module SoOSiM.Components.Scheduler where

import Data.Maybe
import SoOSiM

import SoOSiM.Components.Scheduler.Types
import SoOSiM.Components.ResourceDiscovery.Types
import SoOSiM.Components.MemoryManager.Types as MemoryManager

scheduler schedState (ComponentMsg sender content) = do
  case (fromDynamic content) of
    Just (Instantiate remote@True n cname) -> do
      rdCompId <- fmap fromJust $ componentLookup Nothing "ResourceDiscovery"
      memCompId <- fmap (fromMaybe (error "MemoryManager has no fallback"))  $ componentLookup Nothing "MemoryManager"
      (FoundNodes nIds) <- fmap (fromJust . fromDynamic) $ invoke Nothing rdCompId (toDyn (FindNodes True n))

      memCompIds <- mapM (\nId -> createComponent (Just nId) (Just sender) "MemoryManager") nIds
      mapM_ (\mId -> invokeNoWait Nothing mId (toDyn (MemoryManager.NewState (MemState [] memCompId)))) memCompIds

      compIds <- mapM (\nId -> createComponent (Just nId) (Just sender) cname) nIds
      invokeNoWait Nothing sender (toDyn compIds)
      yield schedState

    Just (Instantiate remote@False n cname) -> do
      compIds <- sequence $ replicate n (createComponent Nothing (Just sender) cname)
      invokeNoWait Nothing sender (toDyn compIds)
      yield schedState

    --Just (Execute cname memCommands) -> do
    --    rdCompId <- fmap fromJust $ componentLookup Nothing "ResourceDiscovery"
    --    --(FoundNode [nodeId]) <- fmap (fromJust . fromDynamic) $ invoke Nothing rdCompId (toDyn (FindNodes 1))
    --    nodeId <- createNode
    --    memCompId <- createComponent (Just nodeId) Nothing "MemoryManager"
    --    mapM_ (invokeNoWait Nothing memCompId . toDyn) memCommands
    --    compId <- createComponent (Just nodeId) (Just sender) cname
    --    invokeNoWait Nothing sender (toDyn compId)
    --    yield schedState
    Nothing -> yield schedState

scheduler schedState _ = yield schedState

--createComponentRequest ::
--  String
--  -> SimM ComponentId
--createComponentRequest s = do
--  schedulerId    <- fmap fromJust $ componentLookup Nothing "ProcessManager"
--  componentIdDyn <- invoke Nothing schedulerId (toDyn s)
--  return (fromJust $ fromDynamic componentIdDyn)

instance ComponentIface SchedulerState where
  initState          = SchedulerState [] []
  componentName _    = "ProcessManager"
  componentBehaviour = scheduler

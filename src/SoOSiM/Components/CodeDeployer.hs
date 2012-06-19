module SoOSiM.Components.CodeDeployer where

import Data.Maybe

import SoOSiM
import SoOSiM.Components.CodeAdapter.Types
import SoOSiM.Components.CodeDeployer.Types
import SoOSiM.Components.MemoryManager.Types as MemoryManager
import SoOSiM.Components.Types.Node

instance ComponentIface DeployerState where
 initState          = DeployerState
 componentName _    = "CodeDeployer"
 componentBehaviour state msg = do
   codeAdapterId <- componentLookup Nothing "CodeAdapter"
   maybe (return state) (componentBehaviourWithIds state msg) codeAdapterId

componentBehaviourWithIds ::
  DeployerState
  -> ComponentInput
  -> CodeDeployerDeps
  -> SimM DeployerState
componentBehaviourWithIds state (ComponentMsg sender contents) deps
  | Just (DeployBlock name code node caller) <- fromDynamic contents
  = do
    -- rsp <- invoke Nothing deps (toDyn (Compile code (nodeArch node)))
    -- invoke Nothing caller (toDyn name)
    memCompId <- fmap fromJust $ componentLookup Nothing "MemoryManager"
    mId <- createComponent (Just (nodeId node)) (Just caller) "MemoryManager"
    invokeNoWait Nothing mId (toDyn (MemoryManager.NewState (MemState [] memCompId)))

    cId <- createComponent (Just (nodeId node)) (Just caller) name
    invokeNoWait Nothing sender (toDyn cId)

    yield state

  | otherwise
  = yield state

componentBehaviourWithIds state _ _ = yield state

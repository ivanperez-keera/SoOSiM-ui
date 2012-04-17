module SoOSiM.Components.MemoryManager where

import Data.IntMap
import SoOSiM

import SoOSiM.Components.MemoryManager.Types
import SoOSiM.Components.MemoryManager.Util

memoryManager :: MemState -> ComponentInput -> SimM MemState
memoryManager s (ComponentMsg senderId msgContent)
  | Just (NewState s') <- fromDynamic msgContent
  = yield s'

  | Just (Register addr sc src) <- fromDynamic msgContent
  = yield $ s {addressLookup = (MemorySource addr sc src):(addressLookup s)}

  | Just (Read addr) <- fromDynamic msgContent
  = do
    let src = checkAddress (fallback s) (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        traceMsg ("Reading addr: " ++ show addr ++ " for component: " ++ show senderId)
        addrVal <- readMemory Nothing addr
        invokeNoWait Nothing senderId addrVal
        yield s
      Just remote -> do
        traceMsg ("Forwarding read of addr: " ++ show addr ++ " to: " ++ show remote)
        response <- invoke Nothing remote msgContent
        invokeNoWait Nothing senderId response
        yield s

  | Just (Write addr val) <- fromDynamic msgContent
  = do
    let src = checkAddress (fallback s) (addressLookup s) addr
    case (sourceId src) of
      Nothing -> do
        traceMsg ("Writing addr: " ++ show addr ++ " for component: " ++ show senderId)
        addrVal <- writeMemory Nothing addr val
        yield s
      Just remote -> do
        traceMsg ("Forwarding write of addr: " ++ show addr ++ " to: " ++ show remote)
        invokeNoWait Nothing remote msgContent
        yield s

memoryManager s _ = return s

instance ComponentIface MemState where
  initState          = MemState [] (error "MemoryManager has no fallback")
  componentName _    = "MemoryManager"
  componentBehaviour = memoryManager

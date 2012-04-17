module SoOSiM.Components.ResourceDiscovery where

import Data.Maybe
import SoOSiM

import SoOSiM.Components.ResourceDiscovery.Types
import SoOSiM.Components.Types.Node

resourceDiscovery rdState (ComponentMsg senderId content)
  | (Just (NewState s')) <- fromDynamic content
  = yield s'

  | (Just (RequestResource fresh n _)) <- fromDynamic content
  = do
    let (ns,coreUsed') = updateState (coreUsed rdState) fresh n
    invokeNoWait Nothing senderId (toDyn (FoundNodes ns))
    yield (RDState coreUsed')

resourceDiscovery rdState _ = yield rdState

updateState :: [(NodeDef,Bool)] -> Bool -> Int -> ([NodeDef],[(NodeDef,Bool)])
updateState s  _ 0 = ([],s)
updateState [] _ _ = ([],[])
updateState ((n,True):ns) True i =
  let (av,ns') = updateState ns True (i-1)
  in (n:av,(n,False):ns')

updateState ((n,False):ns) True i =
  let (av,ns') = updateState ns True i
  in (av,(n,False):ns')

updateState ((n,_):ns) False i =
  let (av,ns') = updateState ns False (i-1)
  in (n:av,(n,False):ns')

instance ComponentIface RDState where
  initState          = RDState []
  componentName _    = "ResourceDiscovery"
  componentBehaviour = resourceDiscovery

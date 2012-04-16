module SoOSiM.Components.ResourceDiscovery where

import Data.Maybe
import SoOSiM

import SoOSiM.Components.ResourceDiscovery.Types

resourceDiscovery hmwState (ComponentMsg _ content)
 | (Just (NewState s')) <- fromDynamic content =
   yield s'
resourceDiscovery rdState _ = yield rdState

instance ComponentIface RDState where
  initState          = RDState [] False
  componentName _    = "ResourceDiscovery"
  componentBehaviour = resourceDiscovery

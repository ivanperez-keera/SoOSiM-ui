{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ResourceDiscovery.Types where

import SoOSiM
import SoOSiM.Components.Types.Code
import SoOSiM.Components.Types.Node

data RDMsg
  = RequestResource Bool Int ResourceReq
  | FoundNodes [NodeDef]
  | NewState  RDState
  deriving Typeable

data RDState
  = RDState
  { coreUsed :: [(NodeDef,Bool)]
  }

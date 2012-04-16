{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ResourceDiscovery.Types where

import SoOSiM

data RDMsg
  = FindNodes Bool Int
  | FoundNodes [NodeId]
  | NewState  RDState
  deriving Typeable

data RDState
  = RDState
  { coreUsed :: [(NodeId,Bool)]
  }

{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ResourceDiscovery.Types where

import SoOSiM

data RDMsg
  = FindNodes Int
  | FoundNode [NodeId]
  | NewState  RDState
  deriving Typeable

data RDState
  = RDState
  { rdComponents :: [ComponentId]
  , coreUsed     :: Bool
  }

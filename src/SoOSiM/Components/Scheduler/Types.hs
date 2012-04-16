{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.Scheduler.Types where

import SoOSiM
import SoOSiM.Components.MemoryManager.Types

data SchedulerMsg
  = NewState SchedulerState
  | Instantiate Bool Int String
  deriving Typeable

data SchedulerState
  = SchedulerState
  { knownNodes :: [NodeId]
  , usedNodes  :: [NodeId]
  }

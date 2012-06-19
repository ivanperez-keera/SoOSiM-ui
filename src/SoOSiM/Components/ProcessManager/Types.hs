{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ProcessManager.Types where

import SoOSiM
import SoOSiM.Components.Types.Code

data ProcessManagerState
  = ProcessManagerState
  { appDataM   :: Maybe AppData
  , runningIds :: [ComponentId]
  }

data SchedulerDependencyIds
  = SchedulerDependencyIds
  { appHndlr     :: ComponentId -- ApplicationHandler
  , resMgr       :: ComponentId -- ResourceManager
  , codeDeployer :: ComponentId -- CodeDeployer
  }

data SchedulerMsg
  = Execute String
  | CreateThreads Int BlockName
  deriving (Typeable)

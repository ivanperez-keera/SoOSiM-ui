{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.MemoryManager.Types where

import SoOSiM

data MemorySource
  = MemorySource
  { baseAddress :: Int
  , scope       :: Int
  , sourceId    :: Maybe ComponentId
  }
  deriving Show


data MemState =
  MemState { addressLookup :: [MemorySource]
           , fallback      :: ComponentId
           }
  deriving Show

data MemCommand = Register Int Int (Maybe ComponentId)
                | Read     Int
                | Write    Int Dynamic
                | SyncWrite Int Dynamic
                | NewState MemState
  deriving Typeable

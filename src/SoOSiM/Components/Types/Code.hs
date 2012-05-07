{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.Types.Code where

import SoOSiM

data AppData = AppData { appBlocks :: [(BlockName, BlockCode, ResourceReq)] }
  deriving (Typeable)

data ResourceReq
  = ResourceReq
  deriving (Typeable)

type BlockName = String
type BlockId   = Int
type BlockCode = String

data AppMsg
  = Compute
  | Done
  deriving (Eq,Typeable)

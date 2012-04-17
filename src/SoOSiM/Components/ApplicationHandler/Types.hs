{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.ApplicationHandler.Types where

import Data.Map

import SoOSiM
import SoOSiM.Components.Types.Code

data AppHndlrMsg
  = AppHndlrMsg String
  | AppHndlr    AppData
  | AppReg      String  AppData
  deriving (Typeable)

data AppHandlerState
  = AppHandlerState
  { appMap :: Map String AppData
  }

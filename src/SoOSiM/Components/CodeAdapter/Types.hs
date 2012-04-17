{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.CodeAdapter.Types where

import SoOSiM
import SoOSiM.Components.Types.Code
import SoOSiM.Components.Types.Node

data TransfomerState = TransfomerState

data CodeAdapterMsg = Compile BlockCode Architecture
 deriving (Typeable)

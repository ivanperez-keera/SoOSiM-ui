module SoOSiM.Components.CodeAdapter where

import SoOSiM
import SoOSiM.Components.CodeAdapter.Types

instance ComponentIface TransfomerState where
  initState          = TransfomerState
  componentName _    = "CodeAdapter"
  componentBehaviour state (ComponentMsg caller contents)
      | Just (Compile code node) <- fromDynamic contents
      -- To be completed
      = yield state
  componentBehaviour state _ = yield state


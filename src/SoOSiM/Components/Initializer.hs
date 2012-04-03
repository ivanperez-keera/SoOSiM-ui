module SoOSiM.Components.Initializer where

import SoOSiM
import SoOSiM.Types

data Initializer = Initializer

initializer ::
  Initializer
  -> ComponentInput
  -> SimM Initializer
initializer s _ = yield s

instance ComponentIface Initializer where
  initState          = Initializer
  componentName _    = "Initializer"
  componentBehaviour = initializer

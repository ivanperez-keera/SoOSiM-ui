module SoOSiM.Components.Types.Node where

import SoOSiM

data NodeDef = NodeDef
  { nodeId   :: NodeId
  , nodeArch :: Architecture
  }

type Architecture = String

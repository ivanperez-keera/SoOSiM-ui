{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.CodeDeployer.Types where

import SoOSiM
import SoOSiM.Components.Types.Code
import SoOSiM.Components.Types.Node

data DeployerState = DeployerState

type CodeDeployerDeps = ComponentId -- CodeAdapterId

data CodeDeployerMsg = DeployBlock String BlockCode NodeDef ComponentId
 deriving (Typeable)

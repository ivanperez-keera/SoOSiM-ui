{-# LANGUAGE TemplateHaskell #-}
-- | Protected reactive fields accessible in the model
module Model.ProtectedModel.ProtectedFields where

-- Internal imports
import Hails.MVC.Model.THFields
import Hails.MVC.Model.ProtectedModel.Reactive

import Model.Model
import qualified Model.ReactiveModel as RM
import Model.ReactiveModel.ModelEvents
import Model.ProtectedModel.ProtectedModelInternals

-- protectedField {- Model field -} {- Field type -}    {- Model name -} {- event name -}
-- protectedField "Language"        [t|Maybe Language|] "Model"          "ModelEvent"

-- | Simulation status (running, paused, halted)
protectedField "Status"     [t|Status|] "Model" "ModelEvent"

-- | Simulation speed (>= 0)
protectedField "Speed"      [t|Float|]  "Model" "ModelEvent"

-- | Fullscreen/windowed mode
protectedField "Fullscreen" [t|Bool|]   "Model" "ModelEvent"

protectedField "SimState" [t|Maybe SimGLState |] "Model" "ModelEvent"

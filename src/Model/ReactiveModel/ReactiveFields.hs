{-# LANGUAGE TemplateHaskell #-}
-- | Reactive fields accessible in the model
module Model.ReactiveModel.ReactiveFields where

-- External imports
import qualified Hails.MVC.Model.ReactiveFields as RFs
import Hails.MVC.Model.ReactiveFields
         (fieldGetter, fieldSetter, preTrue)
import Hails.MVC.Model.THFields

-- Internal imports
import Model.Model
import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.ModelEvents

-- A Field of type A lets us access a reactive field of type a from
-- a Model, and it triggers a ModelEvent
type Field a = RFs.Field a Model ModelEvent

-- reactiveField {- Field name -} {- Field type -}

-- | Simulation status (running, paused, halted)
reactiveField "Status"     [t|Status|]

-- | Simulation speed (>= 0)
reactiveField "Speed"      [t|Float|]

-- | Fullscreen/windowed mode
reactiveField "Fullscreen" [t|Bool|]

reactiveField "SimState" [t|Maybe SimGLState |]

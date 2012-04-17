module SoOSiM.Components.ProcessManager.Util where

import Data.Maybe

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m d f = maybe d f m

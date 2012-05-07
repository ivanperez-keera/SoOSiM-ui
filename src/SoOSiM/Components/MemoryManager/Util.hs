module SoOSiM.Components.MemoryManager.Util where

import Data.Maybe
import SoOSiM

import SoOSiM.Components.MemoryManager.Types

checkAddress ::
  ComponentId
  -> [MemorySource]
  -> Int
  -> MemorySource
checkAddress fbMem sources addr = case (filter containsAddr sources) of
    []    -> (MemorySource undefined undefined (Just fbMem))
    (x:_) -> x
  where
    containsAddr (MemorySource base sc _) = base <= addr && addr < sc

-- | A sample application with just an initializer
module SoOSiM.Samples.Initializer where

-- External imports
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import SoOSiM.Types
import UniqSupply
import Unique

-- Local imports
import SoOSiM.Components.Initializer

simstate :: IO SimState
simstate = do
  supply <- mkSplitUniqSupply 'z'
  let (supply',supply'')       = splitUniqSupply supply
      (node0id:component0id:_) = uniqsFromSupply supply'

      -- Main component and node
      component0CC = CC Running Initializer (error "no parent") [Initialize]
      node0        = Node node0id NodeInfo
                          (Map.fromList [("Initializer",component0id)])
                          (IM.fromList [(getKey component0id,component0CC)])
                          IM.empty

      -- Initial state
      simState = SimState node0id component0id
                          (IM.fromList [(getKey node0id,node0)])
                          supply''
                          (Map.fromList [("Initializer",component0CC)])

  return simState

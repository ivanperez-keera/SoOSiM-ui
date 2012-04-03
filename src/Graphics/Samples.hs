module Graphics.Samples where

import Graphics.MultiCoreStatus as M
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as Map
import SoOSiM
import SoOSiM.Simulator
import SoOSiM.Types
import UniqSupply
import Unique

diagram :: MultiCoreStatus
diagram = MultiCoreStatus [pu1, pu2, pu3] [m1, m2] []

pu1 :: ProcessingUnit
pu1 = ProcessingUnit "PU1" [pu1c1, pu1c2, pu1app1] UnitExpanded

pu3 :: ProcessingUnit
pu3 = ProcessingUnit "PU3" [pu1c1, pu1app1] UnitCollapsed

pu2 :: ProcessingUnit
pu2 = ProcessingUnit "PU2" [pu2c1, pu2c2, pu2app2] UnitExpanded

pu1c1 :: RunningElement
pu1c1 = Component "P2" "C1" M.Idle Nothing

pu1c2 :: RunningElement
pu1c2 = Component "P9" "C2" Active  Nothing

pu1app1 :: RunningElement
pu1app1 = Application "P11" "App1" M.Idle Nothing

pu2c1 :: RunningElement
pu2c1 = Component "P12" "C1" Active Nothing

pu2c2 :: RunningElement
pu2c2 = Component "P14" "C2" Waiting Nothing

pu2app2 :: RunningElement
pu2app2 = Application "P1546" "App2" Waiting Nothing

m1 :: Message
m1 = Message ("PU1", "C2")
             ("PU2", "C2")
             "Fantastich"

m2 :: Message
m2 = Message ("PU2", "C1")
             ("PU2", "App2")
             "Super Fantastich"

simstate :: IO SimState
simstate = do
  supply <- mkSplitUniqSupply 'z'
  let (supply',supply'') = splitUniqSupply supply
  let (node0id:component0id:_) = uniqsFromSupply supply'
  let component0CC = CC Running Initializer (error "no parent") [Initialize]
  let node0 = Node node0id NodeInfo (Map.fromList [("Initializer",component0id)]) (IM.fromList [(getKey component0id,component0CC)]) IM.empty
  let simState = SimState node0id component0id (IM.fromList [(getKey node0id,node0)]) supply'' (Map.fromList [("Initializer",component0CC)])

  return simState

data Initializer = Initializer

initializer ::
  Initializer
  -> ComponentInput
  -> SimM Initializer
initializer s Initialize = do
  yield s

initializer s _ = yield s

instance ComponentIface Initializer where
  initState = Initializer
  componentName _ = "Initializer"
  componentBehaviour = initializer

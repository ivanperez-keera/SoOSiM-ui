module Graphics.Samples where

import Graphics.MultiCoreStatus

diagram :: MultiCoreStatus
diagram = MultiCoreStatus [pu1, pu2, pu3] [m1, m2]

pu1 :: ProcessingUnit
pu1 = ProcessingUnit "PU1" [pu1c1, pu1c2, pu1app1] UnitExpanded

pu3 :: ProcessingUnit
pu3 = ProcessingUnit "PU3" [pu1c1, pu1app1] UnitCollapsed

pu2 :: ProcessingUnit
pu2 = ProcessingUnit "PU2" [pu2c1, pu2c2, pu2app2] UnitExpanded

pu1c1 :: RunningElement
pu1c1 = Component "P2" "C1" Idle Nothing

pu1c2 :: RunningElement
pu1c2 = Component "P9" "C2" Active  Nothing

pu1app1 :: RunningElement
pu1app1 = Application "P11" "App1" Idle Nothing

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

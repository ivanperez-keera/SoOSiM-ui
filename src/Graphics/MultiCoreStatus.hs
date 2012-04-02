module Graphics.MultiCoreStatus where

import Data.Maybe
import Graphics.Types

data MultiCoreStatus = MultiCoreStatus
  { processingUnits :: [ ProcessingUnit ]
  , messages        :: [ Message ]
  , selection       :: [ Name ]
  }
 deriving (Eq, Show)

data ProcessingUnit = ProcessingUnit
  { unitName     :: Name
  , unitElements :: [ RunningElement ]
  , unitStatus   :: UnitStatus
  }
 deriving (Eq, Show)

data UnitStatus = UnitCollapsed
                | UnitExpanded
                | UnitIgnored
 deriving (Eq, Show)

data RunningElement = Component   { elementName       :: Name
                                  , elementKind       :: ElementKind
                                  , elementState      :: ElementState
                                  , elementGhost      :: Maybe QElementName
                                  -- , elementStatistics :: Statistics
                                  }
                    | Application { elementName       :: Name
                                  , elementKind       :: ElementKind
                                  , elementState      :: ElementState
                                  , elementGhost      :: Maybe QElementName
                                  -- , elementStatistics :: Statistics
                                  }
 deriving (Eq, Show)

data ElementState = Active
                  | Waiting
                  | Idle
 deriving (Eq, Show)
 
data Statistics = Statistics
 deriving (Eq, Show)

data Message = Message { sender   :: QElementName
                       , receiver :: QElementName
                       , kind     :: String
                       }
 deriving (Eq, Show)

type QElementName = (Name, Name)
type ElementKind = String

changeState :: QElementName -> ElementState -> MultiCoreStatus -> MultiCoreStatus
changeState (puN, reN) s d = d { processingUnits = ps' }
  where ps' = map upd $ processingUnits d
        upd x = if puN == unitName x then changeStatePU reN s x else x

changeStatePU :: Name -> ElementState -> ProcessingUnit -> ProcessingUnit
changeStatePU e s pu = pu { unitElements = us' }
  where us' = map upd $ unitElements pu
        upd x = if elementName x == e then changeStateRE s x else x

changeStateRE :: ElementState -> RunningElement -> RunningElement
changeStateRE s re = re { elementState = s }

addMessage :: Message -> MultiCoreStatus -> MultiCoreStatus
addMessage m d = d { messages = m : messages d }

removeMessage :: Message -> MultiCoreStatus -> MultiCoreStatus
removeMessage m d = d { messages = ms' }
  where ms' = filter (m /=) $ messages d

findRunningElement :: QElementName -> MultiCoreStatus -> Maybe RunningElement
findRunningElement (pn,en) d =
  findRunningElementInPU en =<< findProcessingUnit pn d
  
findRunningElementInPU :: Name -> ProcessingUnit -> Maybe RunningElement
findRunningElementInPU en =
  listToMaybe . filter ((en ==) . elementName) . unitElements

findProcessingUnit :: Name -> MultiCoreStatus -> Maybe ProcessingUnit
findProcessingUnit pn = listToMaybe . filter ((pn ==) . unitName) . processingUnits

updateProcessingUnit :: ProcessingUnit -> MultiCoreStatus -> MultiCoreStatus
updateProcessingUnit pu d = d { processingUnits = ps' }
 where ps    = processingUnits d
       ps'   = map upd ps
       upd x = if unitName pu == unitName x then pu else x
       pn    = unitName pu

addRunningElement :: Name -> RunningElement -> MultiCoreStatus -> MultiCoreStatus
addRunningElement pn re d = maybe d (upd . addRunningElementPU re) mpu
 where mpu = findProcessingUnit pn d
       upd x = updateProcessingUnit x d

addRunningElementPU :: RunningElement -> ProcessingUnit -> ProcessingUnit
addRunningElementPU re pu = pu { unitElements = re : unitElements pu }

removeRunningElement :: QElementName -> MultiCoreStatus -> MultiCoreStatus
removeRunningElement (pn, en) d = maybe d (upd . removeRunningElementPU en) mpu
 where upd x = updateProcessingUnit x d
       mpu   = findProcessingUnit pn d

removeRunningElementPU :: Name -> ProcessingUnit -> ProcessingUnit
removeRunningElementPU en pu = pu { unitElements = es' }
 where es' = filter ((en /=) . elementName) $ unitElements pu

updateRunningElement :: Name-> RunningElement -> MultiCoreStatus -> MultiCoreStatus
updateRunningElement pn re d = maybe d (upd . updateRunningElementPU re) mpu
 where mpu   = findProcessingUnit pn d
       upd x = updateProcessingUnit x d

updateRunningElementPU :: RunningElement -> ProcessingUnit -> ProcessingUnit
updateRunningElementPU re pu = pu { unitElements = es' }
 where es'   = map upd $ unitElements pu
       upd x = if elementName re == elementName x then re else x

moveRunningElement :: QElementName -> QElementName -> MultiCoreStatus -> MultiCoreStatus
moveRunningElement q1 q2 d = maybe d transport mre
 where mre          = findRunningElement q1 d
       transport re = updateRunningElement (fst q1) (re { elementGhost = Just q2 })
                      $ addRunningElement (fst q2) (re { elementName = snd q2 }) d

toggleStatus :: (Name, Name) -> MultiCoreStatus -> MultiCoreStatus
toggleStatus qname (MultiCoreStatus ps ms s) = MultiCoreStatus ps' ms s
 where ps' = map (toggleStatusPU qname) ps

toggleStatusPU :: (Name, Name) -> ProcessingUnit -> ProcessingUnit
toggleStatusPU (pName, cName) pu
 | unitName pu == pName = ProcessingUnit pName us' st
 | otherwise            = pu
 where us' = map (toggleStatusRE cName) (unitElements pu)
       st  = unitStatus pu

toggleStatusRE :: Name -> RunningElement -> RunningElement
toggleStatusRE n re
 | elementName re == n = re { elementState = toggleStatusS (elementState re) }
 | otherwise           = re

toggleStatusS :: ElementState -> ElementState
toggleStatusS Active  = Waiting
toggleStatusS Waiting = Idle
toggleStatusS Idle    = Active

setSelection :: [Name] -> MultiCoreStatus -> MultiCoreStatus
setSelection n m = m { selection = n }

toggleVisibility :: [Name] -> MultiCoreStatus -> MultiCoreStatus
toggleVisibility [n] st = st { processingUnits = pu' }
 where pu  = processingUnits st
       pu' = map (toggleVisibilityPU n) pu
toggleVisibility _   st = st

toggleVisibilityPU :: Name -> ProcessingUnit -> ProcessingUnit
toggleVisibilityPU n pu = if n == unitName pu then pu' else pu
 where pu' = pu { unitStatus = toggleVisibilityS st }
       st  = unitStatus pu

toggleVisibilityS :: UnitStatus -> UnitStatus
toggleVisibilityS UnitCollapsed = UnitExpanded
toggleVisibilityS UnitExpanded  = UnitCollapsed
toggleVisibilityS UnitIgnored   = UnitIgnored

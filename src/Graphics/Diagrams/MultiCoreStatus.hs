-- | Description of multicore systems and the status
-- that they are in
module Graphics.Diagrams.MultiCoreStatus
   (
   -- * System description
     MultiCoreStatus(..)
   , emptyMultiCoreStatus
   , ProcessingUnit(..)
   , UnitStatus(..)
   , ElementState(..)
   , RunningElement(..)
   , Message(..)
   , Statistics(..)

   -- * Access and manipulation
   , findProcessingUnit
   , findRunningElement
   , toggleVisibility
   )
  where

-- External imports
import Data.Maybe

-- Internal imports
import Graphics.Diagrams.Types

-- | A MultiCore system is a collection of processing units
-- current messages and a selection
data MultiCoreStatus = MultiCoreStatus
  { processingUnits :: [ ProcessingUnit ]
  , messages        :: [ Message ]
  }
 deriving (Eq, Show)

-- | Empty system with no PUs, no messages and no selection
emptyMultiCoreStatus :: MultiCoreStatus
emptyMultiCoreStatus = MultiCoreStatus [] []

-- | A processing unit has a name, a list of components
-- or applications running in it, and a status
data ProcessingUnit = ProcessingUnit
  { unitName     :: Name
  , unitElements :: [ RunningElement ]
  , unitStatus   :: UnitStatus
  }
 deriving (Eq, Show)

-- | A PU can be collapsed (the contents must be hidden
-- from the user), expanded (the contents must be shown)
-- or ignored (the whole unit must be hidden)
data UnitStatus = UnitCollapsed
                | UnitExpanded
                | UnitIgnored
 deriving (Eq, Show)

-- | A Running Element can be an OS component or a user
-- application, both of which have the same attributes.
data RunningElement = Component   { elementName       :: Name
                                  , elementKind       :: ElementKind
                                  , elementState      :: ElementState
                                  , elementGhost      :: Maybe QElementName
                                  , elementStatistics :: Statistics
                                  }
                    | Application { elementName       :: Name
                                  , elementKind       :: ElementKind
                                  , elementState      :: ElementState
                                  , elementGhost      :: Maybe QElementName
                                  , elementStatistics :: Statistics
                                  }
 deriving (Eq, Show)

-- | A running element can be active (running), waiting (sync message)
-- or idle
data ElementState = Active
                  | Waiting
                  | Idle
 deriving (Eq, Show)
 
-- Collected statistics
data Statistics = Statistics { compCyclesRunning :: Int
                             , compCyclesWaiting :: Int
                             , compCyclesIdling  :: Int
                             , compTrace         :: [String]
                             }
 deriving (Eq, Show)

-- | A message has an origin, a destination, and a label
data Message = Message { sender   :: [Name]
                       , receiver :: [Name]
                       , kind     :: String
                       }
 deriving (Eq, Show)

-- | A qualified component name
type QElementName = (Name, Name)

-- | The kind of a running element
type ElementKind = String

-- | Finds a processing unit by name
findProcessingUnit :: Name -> MultiCoreStatus -> Maybe ProcessingUnit
findProcessingUnit pn = listToMaybe . filter ((pn ==) . unitName) . processingUnits

-- | Changes the visibility of an element
toggleVisibility :: [Name] -> MultiCoreStatus -> MultiCoreStatus
toggleVisibility [n] st = st { processingUnits = pu' }
 where pu  = processingUnits st
       pu' = map (toggleVisibilityPU n) pu
toggleVisibility _   st = st

-- | Changes the visibility of an element in a PU
toggleVisibilityPU :: Name -> ProcessingUnit -> ProcessingUnit
toggleVisibilityPU n pu = if n == unitName pu then pu' else pu
 where pu' = pu { unitStatus = toggleVisibilityS st }
       st  = unitStatus pu

-- | Toggles a visibility status
toggleVisibilityS :: UnitStatus -> UnitStatus
toggleVisibilityS UnitCollapsed = UnitExpanded
toggleVisibilityS UnitExpanded  = UnitCollapsed
toggleVisibilityS UnitIgnored   = UnitIgnored

-- -- | Updates the state of a RE in a system
-- changeState :: QElementName -> ElementState -> MultiCoreStatus -> MultiCoreStatus
-- changeState (puN, reN) s d = d { processingUnits = ps' }
--   where ps' = map upd $ processingUnits d
--         upd x = if puN == unitName x then changeStatePU reN s x else x
-- 
-- -- | Updates the state of a RE in a PU
-- changeStatePU :: Name -> ElementState -> ProcessingUnit -> ProcessingUnit
-- changeStatePU e s pu = pu { unitElements = us' }
--   where us' = map upd $ unitElements pu
--         upd x = if elementName x == e then changeStateRE s x else x
-- 
-- -- | Updates the state of a RE in a PU
-- changeStateRE :: ElementState -> RunningElement -> RunningElement
-- changeStateRE s re = re { elementState = s }
-- 
-- -- | Adds a new message to the system
-- addMessage :: Message -> MultiCoreStatus -> MultiCoreStatus
-- addMessage m d = d { messages = m : messages d }
-- 
-- -- | Removes a message from the system
-- removeMessage :: Message -> MultiCoreStatus -> MultiCoreStatus
-- removeMessage m d = d { messages = ms' }
--   where ms' = filter (m /=) $ messages d

-- | Finds a running element by name
findRunningElement :: QElementName -> MultiCoreStatus -> Maybe RunningElement
findRunningElement (pn,en) d =
  findRunningElementInPU en =<< findProcessingUnit pn d
  
-- | Finds a running element by name in a PU
findRunningElementInPU :: Name -> ProcessingUnit -> Maybe RunningElement
findRunningElementInPU en =
  listToMaybe . filter ((en ==) . elementName) . unitElements

-- -- | Updates a processing unit in a system
-- updateProcessingUnit :: ProcessingUnit -> MultiCoreStatus -> MultiCoreStatus
-- updateProcessingUnit pu d = d { processingUnits = ps' }
--  where ps    = processingUnits d
--        ps'   = map upd ps
--        upd x = if unitName pu == unitName x then pu else x
--        pn    = unitName pu
-- 
-- -- | Adds a running element to the system
-- addRunningElement :: Name -> RunningElement -> MultiCoreStatus -> MultiCoreStatus
-- addRunningElement pn re d = maybe d (upd . addRunningElementPU re) mpu
--  where mpu = findProcessingUnit pn d
--        upd x = updateProcessingUnit x d
-- 
-- -- | Adds a running element to a processing unit
-- addRunningElementPU :: RunningElement -> ProcessingUnit -> ProcessingUnit
-- addRunningElementPU re pu = pu { unitElements = re : unitElements pu }
-- 
-- -- | Removes a running element from the system
-- removeRunningElement :: QElementName -> MultiCoreStatus -> MultiCoreStatus
-- removeRunningElement (pn, en) d = maybe d (upd . removeRunningElementPU en) mpu
--  where upd x = updateProcessingUnit x d
--        mpu   = findProcessingUnit pn d
-- 
-- -- | Removes a running element from a processing unit
-- removeRunningElementPU :: Name -> ProcessingUnit -> ProcessingUnit
-- removeRunningElementPU en pu = pu { unitElements = es' }
--  where es' = filter ((en /=) . elementName) $ unitElements pu
-- 
-- -- | Updates a running element in the system
-- updateRunningElement :: Name-> RunningElement -> MultiCoreStatus -> MultiCoreStatus
-- updateRunningElement pn re d = maybe d (upd . updateRunningElementPU re) mpu
--  where mpu   = findProcessingUnit pn d
--        upd x = updateProcessingUnit x d
-- 
-- -- | Updates a running element in a PU
-- updateRunningElementPU :: RunningElement -> ProcessingUnit -> ProcessingUnit
-- updateRunningElementPU re pu = pu { unitElements = es' }
--  where es'   = map upd $ unitElements pu
--        upd x = if elementName re == elementName x then re else x
-- 
-- -- | Moves a RE from one PU to another
-- moveRunningElement :: QElementName -> QElementName -> MultiCoreStatus -> MultiCoreStatus
-- moveRunningElement q1 q2 d = maybe d transport mre
--  where mre          = findRunningElement q1 d
--        transport re = updateRunningElement (fst q1) (re { elementGhost = Just q2 })
--                      $ addRunningElement (fst q2) (re { elementName = snd q2 }) d

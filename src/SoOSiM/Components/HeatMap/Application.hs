{-# LANGUAGE PatternGuards #-}
module SoOSiM.Components.HeatMap.Application where

import Data.Maybe
import qualified Data.IntMap as IM

import SoOSiM
import SoOSiM.Components.MemoryManager.Types
import SoOSiM.Components.ProcessManager.Types

import SoOSiM.Components.HeatMap.Types as HeatMap
import SoOSiM.Components.HeatMap.Util

import SoOSiM.Components.Types.Code

heatMapApplication :: HMState -> ComponentInput -> SimM HMState
-- Initialization behaviour
heatMapApplication hmState (ComponentMsg senderId content)
  | Just Compute <- fromDynamic content
  = do
    let (w,h) = arraySize hmState

    -- Calculate read locations for worker threads
    let rlocs = [ ( dimTrans w h x (0,0)
                  , filter (>= 0) [dimTrans w h x (0,-1), dimTrans w h x (0,1)]
                  , filter (>= 0) [dimTrans w h x (-1,0), dimTrans w h x (1,0)])
                | x <- [0..(w*h)-1]
                ]

    -- Calculate write locations for worker threads
    let wlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]

    -- zero-out memory
    memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"
    invokeNoWait Nothing memManagerId (toDyn (Register 0 (2 * w * h) Nothing))
    mapM_ (\wloc -> invokeNoWait Nothing memManagerId (toDyn (Write wloc (toDyn (0::Float))))) [0..(2*w*h-1)]

    -- Set every 3rd location to 1
    mapM_ (\wloc -> invokeNoWait Nothing memManagerId (toDyn (Write wloc (toDyn (1::Float))))) [ x | x <- [0..(w*h-1)], x `mod` 4 == 0]

    -- Instantiate worker threads
    pmId <- fmap fromJust $ componentLookup Nothing "ProcessManager"
    invokeNoWait Nothing pmId (toDyn $ CreateThreads (length wlocs) "HeatMapWorker")
    let workpackages' = map (\(wloc,rloc) -> HMWorker wloc rloc (transfer hmState)) (zip wlocs rlocs)

    yield $ hmState {workpackages = workpackages'}

  | Just (HeatMap.NewState _) <- fromDynamic content
  = do
    case (workpackages hmState) of
      [] -> do
        invokeNoWait Nothing senderId (toDyn $ HeatMap.NewState HMEmpty)
        yield hmState
      (workPackage:workPackages') -> do
        invokeNoWait Nothing senderId (toDyn $ HeatMap.NewState workPackage)
        invokeNoWait Nothing senderId (toDyn $ Compute)
        yield (hmState {workpackages = workPackages', workers = IM.insert (getKey senderId) Compute (workers hmState)})

  -- Keep track of finished workers
  | Just Done <- fromDynamic content
  = do
    let workers' = IM.insert (getKey senderId) Done (workers hmState)
    if all (== Done) . IM.elems $ workers'
      then do -- All workers are finished
        let (w,h) = arraySize hmState

        -- Calculate read and write locations
        let rlocs = [ dimTrans w h x (0,0) + (w*h) | x <- [0..(w*h)-1]]
        let wlocs = [ dimTrans w h x (0,0) | x <- [0..(w*h)-1]]

        -- locate memory managers
        memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

        -- Copy values from 1 array to the other
        rVals <- mapM (invoke Nothing memManagerId . toDyn . Read) rlocs
        _ <- mapM (\(x,v) -> invokeNoWait Nothing memManagerId (toDyn (Write x v))) (zip wlocs rVals)

        traceMsg (show (map (fromJust . fromDynamic) rVals :: [Float]))

        -- Restart all workers to run next iteration
        let workerIDs = map mkUniqueGrimily . IM.keys . workers $ hmState
        mapM_ (\x -> invokeNoWait Nothing x (toDyn Compute)) workerIDs

        yield $ hmState { workers = IM.map (\_ -> Compute) (workers hmState) }
      else -- Still waiting for some workers
        yield $ hmState { workers = workers' }

heatMapApplication hmState _ = yield hmState


heatMapWorker :: HMWorker -> ComponentInput -> SimM HMWorker
heatMapWorker HMEmpty (ComponentMsg _ content)
  | Just Compute <- fromDynamic content
  = do
    creator <- componentCreator
    (HeatMap.NewState s') <- fmap (fromJust . fromDynamic) $ invoke Nothing creator (toDyn (HeatMap.NewState HMEmpty))
    yield s'

heatMapWorker hmwState (ComponentMsg _ content)
 | (Just Compute) <- fromDynamic content
 = do
    -- Extract configuration
    let (c,vert,hor)   = rdLocs hmwState
    let (dy2i,dx2i,dt) = wtransfer hmwState

    -- Locate memory manager
    memManagerId <- fmap fromJust $ componentLookup Nothing "MemoryManager"

    -- Read array values
    cVal    <- fmap (fromJust . fromDynamic)       $ invoke Nothing memManagerId (toDyn (Read c))
    vertVal <- fmap (map (fromJust . fromDynamic)) $ mapM (invoke Nothing memManagerId . toDyn . Read) vert
    horVal  <- fmap (map (fromJust . fromDynamic)) $ mapM (invoke Nothing memManagerId . toDyn . Read) hor

    -- Calculate value
    let newValV = sum (fromIntegral (length vert) * cVal : vertVal) * dy2i
    let newValH = sum (fromIntegral (length hor)  * cVal : horVal ) * dx2i
    let newVal  = (newValV + newValH) * dt

    -- Write array value
    invokeNoWait Nothing memManagerId (toDyn (Write (wrLoc hmwState) (toDyn newVal)))

    -- Notify creator that we're finished
    creator <- componentCreator
    invokeNoWait Nothing creator (toDyn Done)

    yield hmwState

heatMapWorker hmwState _ = yield hmwState


-- ComponetIface instances
instance ComponentIface HMState where
  initState          = HMState IM.empty [] (2,2) (0.5,0.5,0.5)
  componentName _    = "HeatMap"
  componentBehaviour = heatMapApplication

instance ComponentIface HMWorker where
  initState          = HMEmpty
  componentName _    = "HeatMapWorker"
  componentBehaviour = heatMapWorker

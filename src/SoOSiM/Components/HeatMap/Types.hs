{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.Components.HeatMap.Types where

import SoOSiM
import SoOSiM.Components.Types.Code
import Data.IntMap

data HMMsg = NewState HMWorker
  deriving (Eq, Typeable)

data HMState = HMState
  { workers   :: IntMap AppMsg
  , workpackages :: [HMWorker]
  , arraySize :: (Int,Int)
  , transfer  :: (Float,Float,Float)
  }

data HMWorker
  = HMWorker
  { wrLoc     :: Int
  , rdLocs    :: (Int,[Int],[Int])
  , wtransfer :: (Float,Float,Float)
  }
  | HMEmpty
  deriving (Eq, Typeable)

hmAppData = AppData [("HeatMap","HeatMap",ResourceReq),("HeatMapWorker","HeatMapWorker",ResourceReq)]

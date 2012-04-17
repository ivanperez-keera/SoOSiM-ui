-- | A sample application with just an initializer
module SoOSiM.Samples.Initializer where

import Control.Concurrent.STM
import qualified Data.IntMap as IM
import qualified Data.Map    as Map
import SoOSiM
import SoOSiM.Types
import UniqSupply

import SoOSiM.Components.HeatMap.Application ()
import SoOSiM.Components.HeatMap.Types

import SoOSiM.Components.ApplicationHandler ()
import SoOSiM.Components.ApplicationHandler.Types

import SoOSiM.Components.CodeAdapter ()
import SoOSiM.Components.CodeAdapter.Types

import SoOSiM.Components.CodeDeployer ()
import SoOSiM.Components.CodeDeployer.Types

import SoOSiM.Components.MemoryManager ()
import SoOSiM.Components.MemoryManager.Types

import SoOSiM.Components.ProcessManager ()
import SoOSiM.Components.ProcessManager.Types

import SoOSiM.Components.ResourceDiscovery ()
import SoOSiM.Components.ResourceDiscovery.Types as RD

import SoOSiM.Components.Types.Node

simstate :: IO SimState
simstate = do
    supply <- mkSplitUniqSupply 'z'
    let (supply',supply'')       = splitUniqSupply supply
    let (node0id:component0id:_) = uniqsFromSupply supply'
    statusTV                     <- newTVarIO Running
    stateTV                      <- newTVarIO Initializer
    bufferTV                     <- newTVarIO [Initialize]
    meta                         <- newTVarIO $ SimMetaData 0 0 0 Map.empty Map.empty
    let component0CC             = CC component0id statusTV stateTV component0id bufferTV [] meta
    let node0                    = Node node0id NodeInfo Map.empty (IM.fromList [(getKey component0id,component0CC)]) IM.empty [component0id]
    let simState                 = SimState node0id component0id (IM.fromList [(getKey node0id,node0)]) supply'' Map.empty
    return simState

data Initializer = Initializer

initializer ::
  Initializer
  -> ComponentInput
  -> SimM Initializer
initializer s Initialize = do
  nId <- getNodeId
  newNodes <- sequence (replicate 4 createNode)

  registerComponent (initState :: AppHandlerState)
  registerComponent (initState :: TransfomerState)
  registerComponent (initState :: DeployerState)
  registerComponent (initState :: MemState)
  registerComponent (initState :: ProcessManagerState)
  registerComponent (initState :: RDState)

  appHndlrId <- createComponent (Just nId) Nothing "ApplicationHandler"
  _          <- createComponent (Just nId) Nothing "CodeAdapter"
  _          <- createComponent (Just nId) Nothing "CodeDeployer"
  pmId       <- createComponent (Just nId) Nothing "ProcessManager"
  rdId       <- createComponent (Just nId) Nothing "ResourceDiscovery"

  let ndefs = zipWith NodeDef (nId:newNodes) (repeat "CPU")
  invokeNoWait Nothing rdId (toDyn (RD.NewState (RDState (zip ndefs (repeat True)))))

  registerComponent (initState :: HMState)
  registerComponent (initState :: HMWorker)
  invokeNoWait Nothing appHndlrId (toDyn $ AppReg "HeatMap" hmAppData)

  invokeNoWait Nothing pmId (toDyn $ Execute "HeatMap")

  yield s

initializer s _ = yield s

instance ComponentIface Initializer where
  initState          = Initializer
  componentName _    = "ConfigurationProcess"
  componentBehaviour = initializer

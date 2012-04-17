module SoOSiM.Components.ApplicationHandler where

import Data.Map as Map
import SoOSiM.Components.ApplicationHandler.Types

import SoOSiM

instance ComponentIface AppHandlerState where
  initState          = AppHandlerState empty
  componentName _    = "ApplicationHandler"
  componentBehaviour = applicationHandler

applicationHandler ::
  AppHandlerState
  -> ComponentInput
  -> SimM AppHandlerState
applicationHandler s (ComponentMsg sender content)
  | Just (AppReg appName appData) <- fromDynamic content
  = yield (AppHandlerState $ insert appName appData (appMap s))

  | Just (AppHndlrMsg appName) <- fromDynamic content
  = do
    invokeNoWait Nothing sender (toDyn ((appMap s) Map.! appName))
    yield s

  | otherwise
  = yield s

applicationHandler s _ = yield s

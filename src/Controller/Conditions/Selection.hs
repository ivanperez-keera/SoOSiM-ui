module Controller.Conditions.Selection where

--- import Data.CBRef
import Data.CBMVar
import Control.Monad
-- import Graphics.UI.Gtk
-- import Hails.MVC.Model.ProtectedModel.Reactive

import CombinedEnvironment
import Graphics.MultiCoreStatus
-- import Model.Model

-- For now, this simply changes the state of a component every two seconds
installHandlers :: CEnv -> IO()
installHandlers cenv = void $
   installCallbackCBMVar mcsRef $ condition cenv
  where mcsRef = mcs (view cenv)
  
condition :: CEnv -> IO()
condition cenv = do
  c <- readCBMVar mcsRef
  print (selection c)
  where mcsRef = mcs (view cenv)

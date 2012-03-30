module Controller.Conditions.UpdatePicture where

import Data.CBRef
import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment
import Graphics.MultiCoreStatus

-- For now, this simply changes the state of a component every two seconds
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ timeoutAdd (toggleSt mcsRef >> return True) 2000
  where mcsRef = mcs (view cenv)

toggleSt :: CBRef MultiCoreStatus -> IO()
toggleSt mcsRef =
  atomicModifyCBRef mcsRef (\m -> (toggleStatus ("PU1", "P2") m, ()))

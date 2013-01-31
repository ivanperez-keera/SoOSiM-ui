-- | Closes the application when the user closes the main window
-- or clicks on Quit in the File menu
module Controller.Conditions.InitialiseExample
   (installHandlers)
  where

import Control.Monad

import CombinedEnvironment
-- External libraries
import Hails.MVC.Model.ProtectedModel.Reactive

-- Internal libraries
import Data.History
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
import Graphics.UI.Gtk.Display.SoOSiMState
import Model.Model
import Model.SystemStatus
-- import SoOSiM.Samples.Initializer
import SoOSiM.Examples.Example1

-- | Closes the application when the user closes the main window
-- or clicks on Quit in the File menu
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  ss         <- simstate 
  initialMcs <- updateFromSimState emptyMultiCoreStatus ss
  let initialSystemStatus = SystemStatus (historyNew initialMcs) []
  setter simStateField (model cenv) $ Just $ SimGLState initialSystemStatus ss simstate []
  soosimSetMCS soosim (Just initialMcs)
  soosimSetSimState soosim (Just ss)
  soosimSetOver soosim (Just [])
  soosimSetSelection soosim (Just [])
  where soosim = soosimView (view cenv)

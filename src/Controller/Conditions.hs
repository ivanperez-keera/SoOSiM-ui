module Controller.Conditions where

import CombinedEnvironment

import qualified Controller.Conditions.Quit         as Quit
import qualified Controller.Conditions.Selection    as Selection
import qualified Controller.Conditions.Speed        as Speed
import qualified Controller.Conditions.UpdateStatus as Update

installHandlers :: CEnv -> IO ()
installHandlers cenv = do
  Quit.installHandlers      cenv
  Selection.installHandlers cenv
  Speed.installHandlers     cenv
  Update.installHandlers    cenv

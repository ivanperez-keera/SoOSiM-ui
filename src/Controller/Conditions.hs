module Controller.Conditions where

import CombinedEnvironment

import qualified Controller.Conditions.Quit as Quit

installHandlers :: CEnv -> IO ()
installHandlers cenv = do
  Quit.installHandlers cenv

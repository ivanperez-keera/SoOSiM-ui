module Controller.Conditions where

import CombinedEnvironment

import qualified Controller.Conditions.Quit          as Quit
import qualified Controller.Conditions.UpdatePicture as Update
import qualified Controller.Conditions.Speed         as Speed

installHandlers :: CEnv -> IO ()
installHandlers cenv = do
  Quit.installHandlers   cenv
  Update.installHandlers cenv
  Speed.installHandlers  cenv

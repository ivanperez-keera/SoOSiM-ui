-- | Installs all the conditions used by the system. They are, essentially,
-- handlers attached to GUI events or Model events
module Controller.Conditions where

import CombinedEnvironment

import qualified Controller.Conditions.Fullscreen        as Fullscreen
import qualified Controller.Conditions.Quit              as Quit
import qualified Controller.Conditions.Selection         as Selection
import qualified Controller.Conditions.Speed             as Speed
import qualified Controller.Conditions.Step              as Step
import qualified Controller.Conditions.UpdateStatus      as Update
import qualified Controller.Conditions.InfoSelectionArea as InfoSel
import qualified Controller.Conditions.InfoBasicInfo     as InfoBasic

-- | Installs the condition handlers that enforce the system's conditions both
-- from the view to the model and from the model to the view.
installHandlers :: CEnv -> IO ()
installHandlers cenv = do
  Fullscreen.installHandlers cenv
  Quit.installHandlers       cenv
  Selection.installHandlers  cenv
  Speed.installHandlers      cenv
  Step.installHandlers       cenv
  Update.installHandlers     cenv
  InfoSel.installHandlers    cenv
  InfoBasic.installHandlers  cenv

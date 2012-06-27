-- | Installs all the conditions used by the system. They are, essentially,
-- handlers attached to GUI events or Model events
module Controller.Conditions where

import CombinedEnvironment

import qualified Controller.Conditions.Fullscreen             as Fullscreen
import qualified Controller.Conditions.FlowChartWindowVisible as FlowChartWindowVisible
import qualified Controller.Conditions.InfoSelectionArea      as InfoSel
import qualified Controller.Conditions.InfoBasicInfo          as InfoBasic
import qualified Controller.Conditions.InfoTooltip            as InfoTooltip
import qualified Controller.Conditions.Quit                   as Quit
import qualified Controller.Conditions.Selection              as Selection
import qualified Controller.Conditions.Speed                  as Speed
import qualified Controller.Conditions.Step                   as Step
import qualified Controller.Conditions.UpdateStatus           as Update
import qualified Controller.Conditions.ShowState              as ShowState
import qualified Controller.Conditions.InitialiseExample      as LoadExample

-- | Installs the condition handlers that enforce the system's conditions both
-- from the view to the model and from the model to the view.
installHandlers :: CEnv -> IO ()
installHandlers =
  Fullscreen.installHandlers ## 
  FlowChartWindowVisible.flowChartWindowVisibleCondition ## 
  Quit.installHandlers         ## 
  ShowState.installHandlers    ## 
  LoadExample.installHandlers  ## 
  Selection.installHandlers    ## 
  Speed.installHandlers        ## 
  Step.installHandlers         ## 
  Update.installHandlers       ## 
  InfoSel.installHandlers      ## 
  InfoBasic.installHandlers    ## 
  InfoTooltip.installHandlers

(##) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m c
(##) f1 f2 x = f1 x >> f2 x

installCondition :: CEnv -> (CEnv -> IO()) -> IO ()
installCondition cenv f = f cenv

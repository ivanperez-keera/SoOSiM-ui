module Controller.Conditions.FlowChartWindowVisible where

-- External imports
import Control.Monad
import Graphics.UI.Gtk

-- Local imports
import CombinedEnvironment

flowChartWindowVisibleCondition :: CEnv -> IO()
flowChartWindowVisibleCondition cenv = void $ do
  menu <- showFlowChartMenuItem $ uiBuilder $ view cenv
  menu `on` menuItemActivate $ flowChartWindowConditionEnforcer cenv

flowChartWindowConditionEnforcer :: CEnv -> IO()
flowChartWindowConditionEnforcer cenv = void $ do
  window  <- flowChartWindow $ uiBuilder $ view cenv
  visible <- get window widgetVisible
  if visible
   then widgetHide window
   else widgetShowAll window

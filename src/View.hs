-- | Contains basic operations related to the GUI
module View
  ( module View
  , module Exported
  , SimGLVar
  , SimGLState(..)
  )
  where

-- External libraries
import Data.CBMVar
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView
import Hails.MVC.View.GladeView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import Config.Config
import Data.History
import Graphics.Diagrams.MultiCoreStatus
import Graphics.Diagrams.Transformations.SimState2MultiCoreStatus
import Model.SystemStatus
import SoOSiM.Samples.Initializer
import View.InitAnimationArea
import View.InitIconsInfoArea
import View.Tooltips
import View.Objects as Builder

instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = uiBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { uiBuilder    :: Builder
  , mcs          :: SimGLVar
  }

-- | Initialised the glade GUI and all the view components that are not
-- included directly in it
createView :: IO View
createView = do

  -- Certain aspects of the visual interface can be configured with a config file
  cfg <- readConfigFile

  bldr <- loadInterface
  ss   <- simstate 
  initialMcs <- updateFromSimState emptyMultiCoreStatus ss
  let initialSystemStatus = SystemStatus (historyNew initialMcs) []
  msc  <- newCBMVar $ SimGLState initialSystemStatus ss []

  w <- Builder.mainWindow bldr
  widgetShowAll w

  initialiseAnimationArea cfg msc bldr

  initIconsInfoArea bldr

  initialiseTooltips bldr

  return
    View
      { uiBuilder = bldr
      , mcs       = msc
      }

mainWindow :: View -> IO Window
mainWindow = Builder.mainWindow . uiBuilder

animationViewport :: View -> IO Viewport
animationViewport = Builder.animationViewport . uiBuilder

pauseToolBtn :: View -> IO ToolButton
pauseToolBtn = Builder.pauseToolBtn . uiBuilder

runToolBtn :: View -> IO ToolButton
runToolBtn = Builder.runToolBtn . uiBuilder

runSlowToolBtn :: View -> IO ToolButton
runSlowToolBtn = Builder.runSlowToolBtn . uiBuilder

stopToolBtn :: View -> IO ToolButton
stopToolBtn = Builder.stopToolBtn . uiBuilder

slowDownToolBtn :: View -> IO ToolButton
slowDownToolBtn = Builder.slowDownToolBtn . uiBuilder

speedUpToolBtn :: View -> IO ToolButton
speedUpToolBtn = Builder.speedUpToolBtn . uiBuilder

stepForwardToolBtn :: View -> IO ToolButton
stepForwardToolBtn = Builder.stepForwardToolBtn . uiBuilder

stepForwardSmallToolBtn :: View -> IO ToolButton
stepForwardSmallToolBtn = Builder.stepForwardSmallToolBtn . uiBuilder

stepBackToolBtn :: View -> IO ToolButton
stepBackToolBtn = Builder.stepBackToolBtn . uiBuilder

fullScreenToolBtn :: View -> IO ToolButton
fullScreenToolBtn = Builder.fullScreenToolBtn . uiBuilder

fullScreenMenuItem :: View -> IO ImageMenuItem
fullScreenMenuItem = Builder.fullScreenMenuItem . uiBuilder

quitMenuItem :: View -> IO ImageMenuItem
quitMenuItem = Builder.quitMenuItem . uiBuilder

showFlowChartMenuItem :: View -> IO MenuItem
showFlowChartMenuItem = Builder.showFlowChartMenuItem . uiBuilder

menuBar :: View -> IO MenuBar
menuBar = Builder.menuBar . uiBuilder

speedScale :: View -> IO HScale
speedScale = Builder.speedScale . uiBuilder

infoNotebook :: View -> IO Notebook
infoNotebook = Builder.infoNotebook . uiBuilder

infoSelNotebook :: View -> IO Notebook
infoSelNotebook = Builder.infoSelNotebook . uiBuilder

overviewEventBox :: View -> IO EventBox
overviewEventBox = Builder.overviewEventBox . uiBuilder

infoIconView :: View -> IO IconView
infoIconView = Builder.infoIconView . uiBuilder

infoTextView :: View -> IO TextView
infoTextView = Builder.infoTextView . uiBuilder

traceTextView :: View -> IO TextView
traceTextView = Builder.traceTextView . uiBuilder

statusLbl :: View -> IO Label
statusLbl = Builder.statusLbl . uiBuilder

-- Flowchart Window
flowChartWindow :: View -> IO Window
flowChartWindow = Builder.flowChartWindow . uiBuilder

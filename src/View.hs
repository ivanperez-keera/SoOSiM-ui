{-# LANGUAGE TemplateHaskell #-}
-- | Contains basic operations related to the GUI
module View
  ( module View
  , module Exported
  )
  where

-- External libraries
import Control.Monad
import Graphics.UI.Gtk
import Hails.MVC.View.GtkView
import Graphics.UI.Gtk.Extra.BuilderTH
import Hails.MVC.View.GladeView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import Config.Config
import Graphics.UI.Gtk.Display.GlossIO
import Graphics.UI.Gtk.Display.SoOSiMState
import View.InitAnimationArea
import View.InitIconsInfoArea
import View.Tooltips
import View.Objects as Builder

-- | Initialises the GUI. This must be called before
-- any other GUI operation.
initView :: IO ()
initView = void initGUI

-- | Starts a thread for the view.
startView :: IO ()
startView = mainGUI

-- | Executes an operation on the view thread synchronously
onViewSync :: IO a -> IO a
onViewSync = postGUISync

-- | Executes an operation on the view thread asynchronously
onViewAsync :: IO () -> IO ()
onViewAsync = postGUIAsync

-- | Destroys the view thread
destroyView :: IO ()
destroyView = mainQuit


instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = uiBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { uiBuilder    :: Builder
  , soosimView   :: SoOSiMState
  , thumbView    :: GlossIO
  }

-- | Initialised the glade GUI and all the view components that are not
-- included directly in it
createView :: IO View
createView = do

  -- Certain aspects of the visual interface can be configured with a config file
  cfg <- readConfigFile

  -- Load Glade builder
  bldr <- loadInterface

  -- Intialise interface
  widgetShowAll =<< Builder.mainWindow bldr
  (soosim, thumb) <- initialiseAnimationArea cfg bldr
  initIconsInfoArea bldr
  initialiseTooltips bldr

  return View
           { uiBuilder  = bldr
           , soosimView = soosim
           , thumbView  = thumb
           }

-- gtkViewAccessor element name type name
gtkViewAccessor "Builder" "uiBuilder" "mainWindow"           "Window"
gtkViewAccessor "Builder" "uiBuilder" "animationViewport"    "Viewport"
gtkViewAccessor "Builder" "uiBuilder" "pauseToolBtn"         "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "runToolBtn"           "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "runSlowToolBtn"       "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "stopToolBtn"          "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "slowDownToolBtn"      "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "speedUpToolBtn"       "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "stepForwardToolBtn"   "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "stepForwardSmallToolBtn"   "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "stepBackToolBtn"      "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "fullScreenToolBtn"    "ToolButton"
gtkViewAccessor "Builder" "uiBuilder" "fullScreenMenuItem"   "ImageMenuItem"
gtkViewAccessor "Builder" "uiBuilder" "quitMenuItem"         "ImageMenuItem"
gtkViewAccessor "Builder" "uiBuilder" "showFlowChartMenuItem" "MenuItem"
gtkViewAccessor "Builder" "uiBuilder" "menuBar"              "MenuBar"
gtkViewAccessor "Builder" "uiBuilder" "speedScale"           "HScale"
gtkViewAccessor "Builder" "uiBuilder" "infoNotebook"         "Notebook"
gtkViewAccessor "Builder" "uiBuilder" "infoSelNotebook"      "Notebook"
gtkViewAccessor "Builder" "uiBuilder" "overviewEventBox"     "EventBox"
gtkViewAccessor "Builder" "uiBuilder" "fixed1"               "Fixed"
gtkViewAccessor "Builder" "uiBuilder" "infoIconView"         "IconView"
gtkViewAccessor "Builder" "uiBuilder" "infoTextView"         "TextView"
gtkViewAccessor "Builder" "uiBuilder" "traceTextView"        "TextView"
gtkViewAccessor "Builder" "uiBuilder" "statusLbl"            "Label"

-- Flowchart Window
gtkViewAccessor "Builder" "uiBuilder" "flowChartWindow"   "Window"

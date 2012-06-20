{-# LANGUAGE TemplateHaskell #-}
-- | GUI loading and accessing GUI elements from a glade builder
module View.Objects where

-- External imports
import Graphics.UI.Gtk
import Hails.Graphics.UI.Gtk.Builder
import Hails.Graphics.UI.Gtk.THBuilderAccessor

-- Internal imports
import Paths

loadInterface :: IO Builder
loadInterface = loadDefaultInterface getDataFileName

-- gtkBuilderAccessor element name type name
gtkBuilderAccessor "mainWindow"           "Window"
gtkBuilderAccessor "animationViewport"    "Viewport"
gtkBuilderAccessor "pauseToolBtn"         "ToolButton"
gtkBuilderAccessor "runToolBtn"           "ToolButton"
gtkBuilderAccessor "runSlowToolBtn"       "ToolButton"
gtkBuilderAccessor "stopToolBtn"          "ToolButton"
gtkBuilderAccessor "slowDownToolBtn"      "ToolButton"
gtkBuilderAccessor "speedUpToolBtn"       "ToolButton"
gtkBuilderAccessor "stepForwardToolBtn"   "ToolButton"
gtkBuilderAccessor "stepForwardSmallToolBtn"   "ToolButton"
gtkBuilderAccessor "stepBackToolBtn"      "ToolButton"
gtkBuilderAccessor "fullScreenToolBtn"    "ToolButton"
gtkBuilderAccessor "fullScreenMenuItem"   "ImageMenuItem"
gtkBuilderAccessor "quitMenuItem"         "ImageMenuItem"
gtkBuilderAccessor "showFlowChartMenuItem" "MenuItem"
gtkBuilderAccessor "menuBar"              "MenuBar"
gtkBuilderAccessor "speedScale"           "HScale"
gtkBuilderAccessor "infoNotebook"         "Notebook"
gtkBuilderAccessor "infoSelNotebook"      "Notebook"
gtkBuilderAccessor "overviewEventBox"     "EventBox"
gtkBuilderAccessor "infoIconView"         "IconView"
gtkBuilderAccessor "infoTextView"         "TextView"
gtkBuilderAccessor "traceTextView"        "TextView"
gtkBuilderAccessor "statusLbl"            "Label"

-- Flowchart Window
gtkBuilderAccessor "flowChartWindow"   "Window"

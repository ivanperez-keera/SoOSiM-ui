{-# LANGUAGE TemplateHaskell #-}
-- | GUI loading and access to GUI elements from a glade builder
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
gtkBuilderAccessor "window1"              "Window"
gtkBuilderAccessor "scrolledwindow1"      "ScrolledWindow"
gtkBuilderAccessor "viewport1"            "Viewport"
gtkBuilderAccessor "viewport2"            "Viewport"
gtkBuilderAccessor "image1"               "Image"
gtkBuilderAccessor "image2"               "Image"
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
gtkBuilderAccessor "menuBar"              "MenuBar"
gtkBuilderAccessor "hscale1"              "HScale"
gtkBuilderAccessor "vpaned1"              "VPaned"
gtkBuilderAccessor "notebook1"            "Notebook"
gtkBuilderAccessor "notebook2"            "Notebook"
gtkBuilderAccessor "infoSelNotebook"      "Notebook"
gtkBuilderAccessor "overviewEventBox"     "EventBox"
gtkBuilderAccessor "infoIconView"         "IconView"
gtkBuilderAccessor "infoTextView"         "TextView"
gtkBuilderAccessor "traceTextView"        "TextView"
gtkBuilderAccessor "statusLbl"            "Label"

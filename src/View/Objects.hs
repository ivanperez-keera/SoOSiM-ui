{-# LANGUAGE TemplateHaskell #-}
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
gtkBuilderAccessor "image1"               "Image"
gtkBuilderAccessor "image2"               "Image"
gtkBuilderAccessor "pauseToolBtn"         "ToolButton"
gtkBuilderAccessor "runToolBtn"           "ToolButton"
gtkBuilderAccessor "stopToolBtn"          "ToolButton"
gtkBuilderAccessor "slowDownToolBtn"      "ToolButton"
gtkBuilderAccessor "speedUpToolBtn"       "ToolButton"
gtkBuilderAccessor "stepForwardToolBtn"   "ToolButton"
gtkBuilderAccessor "fullScreenToolBtn"    "ToolButton"
gtkBuilderAccessor "fullScreenMenuItem"   "ImageMenuItem"
gtkBuilderAccessor "quitMenuItem"         "ImageMenuItem"
gtkBuilderAccessor "menuBar"              "MenuBar"
gtkBuilderAccessor "hscale1"              "HScale"

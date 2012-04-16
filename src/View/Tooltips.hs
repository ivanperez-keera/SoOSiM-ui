module View.Tooltips where

import Graphics.UI.Gtk
import View.Objects

initialiseTooltips :: Builder -> IO Tooltips
initialiseTooltips bldr = do
 tlt <- tooltipsNew
 
 run <- runToolBtn bldr
 tooltipsSetTip tlt run "Run" "T1"

 sl <- slowDownToolBtn bldr
 tooltipsSetTip tlt sl "Decrease simulation speed" "T2"

 sp <- speedUpToolBtn bldr
 tooltipsSetTip tlt sp "Increase simulation speed" "T3"

 st <- stopToolBtn bldr
 tooltipsSetTip tlt st "Stop" "T4"

 ps <- pauseToolBtn bldr
 tooltipsSetTip tlt ps "Pause" "T5"

 se <- stepForwardToolBtn bldr
 tooltipsSetTip tlt se "One step forward" "T6"

 fs <- fullScreenToolBtn bldr
 tooltipsSetTip tlt fs "Toggle fullscreen" "T7"

 fsM <- fullScreenMenuItem bldr
 tooltipsSetTip tlt fsM "Toggle fullscreen" "T8"

 sc <- hscale1 bldr
 tooltipsSetTip tlt sc "Simulation speed (steps/second)" "T9"

 tooltipsEnable tlt
 return tlt

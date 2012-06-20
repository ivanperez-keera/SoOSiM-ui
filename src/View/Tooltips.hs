-- | Includes a function to initialise the tooltips and return them.
--
--  FIXME: the function receives a tooltips object and an UI object and, using a table
--  of UI Accessors, it installs the tooltips.
module View.Tooltips where

import Graphics.UI.Gtk
import View.Objects

-- Creates a Tooltips element using a builder and a known table of accessors to
-- widgets from the builder.
--
-- FIXME: Move to hails || to hails.view.defaultView
initialiseTooltips :: Builder -> IO Tooltips
initialiseTooltips bldr = do
  tlts <- tooltipsNew
  initialiseTooltips' bldr tlts
  tooltipsEnable tlts
  return tlts

-- Installs tooltips on a Tooltips element using a builder and a known table of
-- accessors to widgets from the builder.
--
-- FIXME: Move to hails?
initialiseTooltips' :: Builder -> Tooltips -> IO ()
initialiseTooltips' bldr tlts = do
  mapM_ (installTooltip bldr tlts) tooltipsWithIds
 where tooltipsWithIds = zip tooltipsTable identifiers
       identifiers     = map (("T" ++) . show) [1..]
 
-- Installs one tooltip on a tooltips widget using a builder, an accessor from that builder,
-- a tooltip string and an identifier
--
-- FIXME: Move to hails?
installTooltip :: Builder -> Tooltips -> ((Builder -> IO Widget, String), String) -> IO ()
installTooltip bldr tlts ((accessor, string), identifier) = do
  widget <- accessor bldr
  tooltipsSetTip tlts widget string identifier

-- | The list of tooltips we know about and use
tooltipsTable :: [(Builder -> IO Widget, String)]
tooltipsTable =
 [ (asWidget runToolBtn,         "Run"                             )
 , (asWidget slowDownToolBtn,    "Decrease simulation speed"       )
 , (asWidget speedUpToolBtn,     "Increase simulation speed"       )
 , (asWidget stopToolBtn,        "Stop"                            )
 , (asWidget pauseToolBtn,       "Pause"                           )  
 , (asWidget stepForwardToolBtn, "One step forward"                ) 
 , (asWidget fullScreenToolBtn,  "Toggle fullscreen"               )
 , (asWidget hscale1,            "Simulation speed (steps/second)" )
 ]
 where -- Transforms an accessor of a type in WidgetClass into a widget
       -- accessor
       asWidget :: WidgetClass a => (b -> IO a) -> (b -> IO Widget)
       asWidget x = fmap toWidget . x

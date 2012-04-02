-- | Contains basic operations related to the GUI
module View
  (module View
  , module Exported)
  where

-- External libraries
import Data.CBMVar
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView
import Hails.MVC.View.GladeView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import View.Objects
import View.InitAnimationArea
import View.InitArtwork

import Graphics.Samples
import Graphics.MultiCoreStatus

instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = uiBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { uiBuilder    :: Builder
  , mcs          :: CBMVar MultiCoreStatus
  }

createView :: IO View
createView = do
  bldr <- loadInterface
  msc  <- newCBMVar diagram

  w <- window1 bldr
  widgetShowAll w
  initialiseAnimationArea msc bldr
  initialiseArtwork       bldr

  return
    View
      { uiBuilder    = bldr
      , mcs          = msc
      }

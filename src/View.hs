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
import SoOSiM.Types (SimState)

-- Internal libraries
import Graphics.MultiCoreStatus
import SoOSiM.Samples.Initializer
import View.Objects
import View.InitAnimationArea
import View.InitArtwork

instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = uiBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { uiBuilder    :: Builder
  , mcs          :: CBMVar (MultiCoreStatus, SimState)
  }

createView :: IO View
createView = do
  bldr <- loadInterface
  ss <- simstate 
  msc <- newCBMVar (emptyMultiCoreStatus, ss)

  w <- window1 bldr
  widgetShowAll w
  initialiseAnimationArea msc bldr
  initialiseArtwork       bldr

  return
    View
      { uiBuilder    = bldr
      , mcs          = msc
      }

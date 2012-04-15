-- | Contains basic operations related to the GUI
module View
  ( module View
  , module Exported
  , SimGlVar
  , SimGlSt
  )
  where

-- External libraries
import Data.CBMVar
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView
import Hails.MVC.View.GladeView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import Graphics.Diagrams.MultiCoreStatus
import SoOSiM.Samples.Initializer
import View.Objects
import View.InitAnimationArea

instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = uiBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { uiBuilder    :: Builder
  , mcs          :: SimGlVar
  }

-- | Initialised the glade GUI and all the view components that are not
-- included directly in it
createView :: IO View
createView = do
  bldr <- loadInterface
  ss   <- simstate 
  msc  <- newCBMVar (emptyMultiCoreStatus, ss, initialViewState, [])


  -- nb <- closeableNotebookNew
  -- lbl1 <- labelNew $ Just "Heiiiiii"
  -- lbl2 <- labelNew $ Just "Haiiiiii"
  -- closeableNotebookAppendPage nb lbl1 "Test1"
  -- closeableNotebookAppendPage nb lbl2 "Test2"

  -- widgetShowAll nb

  -- vp <- vpaned1 bldr

  -- panedAdd2 vp nb
  -- widgetShowAll nb

  -- btn <- buttonNewWithLabel "Hi"

  -- widgetSetSizeRequest btn 30 20
  -- widgetShow btn

  -- nb <- notebook1 bldr

  -- mwdgt <- notebookGetNthPage nb 0
  -- case mwdgt of
  --  Nothing   -> putStrLn "No 0 page"
  --  Just wdgt -> do notebookSetTabLabelPacking nb wdgt PackRepel PackStart
  --                  notebookSetTabLabel nb wdgt btn
  --                  putStrLn . show =<< notebookQueryTabLabelPacking nb wdgt 
  --                  widgetSetSizeRequest btn 30 20
  --                  -- notebookSetTabLabelPacking nb wdgt PackNatural PackStart

  -- containerResizeChildren nb
  -- -- widgetQueueResize nb
  -- -- widgetQueueResize btn

  w <- window1 bldr
  widgetShowAll w

  initialiseAnimationArea msc bldr

  return
    View
      { uiBuilder    = bldr
      , mcs          = msc
      }

-- | Presents the SimState to the user and updates it with input events
module View.InitAnimationArea 
    (initialiseAnimationArea)
  where

-- External imports
import Graphics.UI.Gtk

-- Local imports
import Config.Config
import View.Objects

import Graphics.UI.Gtk.Display.GlossIO
import Graphics.UI.Gtk.Display.GlossIOThumbnail
import Graphics.UI.Gtk.Display.SoOSiMState

-- | Initialises the opengl area with a picture
initialiseAnimationArea :: Config -> Builder -> IO (SoOSiMState, GlossIO)
initialiseAnimationArea cfg bldr = do
  vp <- animationViewport bldr
  fx <- fixed1 bldr

  -- Paint animation inside viewport
  mainWdgt <- soosimStateNew cfg
  containerAdd vp mainWdgt

  wdgt <- glossIOThumbnailNew mainWdgt
  fixedPut fx wdgt (0,0)

  return (mainWdgt, wdgt)

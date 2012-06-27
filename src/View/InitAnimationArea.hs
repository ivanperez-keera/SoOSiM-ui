-- | Presents the SimState to the user and updates it with input events
module View.InitAnimationArea 
        (SimGLVar, SimGLState(..), initialViewState, initialiseAnimationArea)
       where

-- External imports
import Graphics.UI.Gtk -- (Builder, fixedPut, e)

-- Local imports
import Config.Config
import Config.Preferences
import View.Objects
import View.Animation
import View.InitThumbnail
import View.InitMainPictureArea
import Graphics.UI.Gtk.Display.GlossIO
import Graphics.UI.Gtk.Display.SoOSiMState

-- | Initialises the opengl area with a picture
initialiseAnimationArea :: Config -> Builder -> IO (SoOSiMState, GlossIO)
initialiseAnimationArea cfg bldr = do
  vp <- animationViewport bldr
  fx <- fixed1 bldr

  -- Paint animation inside viewport
  mainWdgt <- drawPic cfg
  containerAdd vp mainWdgt

  wdgt <- drawThumb mainWdgt
  fixedPut fx wdgt (0,0)

  return (mainWdgt, wdgt)

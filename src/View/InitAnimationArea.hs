-- | Presents the SimState to the user and updates it with input events
module View.InitAnimationArea 
        (SimGLVar, SimGLState(..), initialViewState, initialiseAnimationArea)
       where

-- External imports
import Graphics.UI.Gtk (Builder)

-- Local imports
import Config.Config
import Config.Preferences
import View.Objects
import View.Animation
import View.InitThumbnail
import View.InitMainPictureArea

-- | Initialises the opengl area with a picture
initialiseAnimationArea :: Config -> SimGLVar -> Builder -> IO ()
initialiseAnimationArea cfg mcs bldr = do
  vp <- animationViewport bldr
  ev <- overviewEventBox bldr

  -- Paint thumbnail inside eventbox with the viewport size for reference
  drawThumb cfg mcs ev vp

  -- Paint animation inside viewport
  drawPic cfg mcs vp

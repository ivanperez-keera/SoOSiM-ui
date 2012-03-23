module View.InitArtwork where

-- External imports
import Graphics.UI.Gtk hiding (Color)

-- Local imports
import View.Objects
import Paths

initialiseArtwork :: Builder -> IO ()
initialiseArtwork bldr = do
  return ()
  -- im1 <- image1 bldr
  -- im2 <- image2 bldr

  -- imageSetFromFile im1 =<< getDataFileName "hand-code-32.png"
  -- imageSetFromFile im2 =<< getDataFileName "hand-message-32.png"

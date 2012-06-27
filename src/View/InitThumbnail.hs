{-# LANGUAGE PackageImports  #-}
-- | Presents the SimState to the user and updates it with input events
module View.InitThumbnail where

-- External imports
import             Data.CBMVar
import             Data.Maybe
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Animate
import             Graphics.UI.Gtk (WidgetClass, widgetGetSize)

-- Local imports
import Config.Config
import Config.Preferences
import Graphics.UI.Gtk.Display.GlossIO
import View.Animation

-- Local imports: basic types
import Graphics.Diagrams.Types ( BoxDescription )
import Graphics.Gloss.AdvancedShapes.Boxes

-- | Draws a thumbnail of the main animation. The thumbnail has the same
-- picture but includes a red box showing the area that is shown on the main
-- panel. It's also not sensitive to user input.
drawThumb :: GlossIOClass g => g -> IO GlossIO
drawThumb glossMain =  do
  gloss <- glossIONewWithAnimation initialThumbnailSize (makeThumbnail glossMain)
  glossIOSetSensitive gloss False
  return gloss

-- | Convert our state to a smaller thumbnail 
makeThumbnail :: GlossIOClass g => g -> a -> IO Picture
makeThumbnail glossMain _ = do
  picM  <- glossIOGetPicture glossMain
  let pic = fromMaybe (Pictures []) picM
  sz   <- widgetGetSize glossMain
  zoom <- glossIOGetZoom glossMain
  diff <- glossIOGetOrig glossMain
  return $ Pictures 
             [ uncurry translate thumbCoords $ scale thumbScale thumbScale pic
             , translate thumbX thumbY $ paintZoomBox zoom diff sz 
             ]

-- Paints the zoom box for a given scale, origin and container size
paintZoomBox :: Float -> Point -> (Int, Int) -> Picture
paintZoomBox scale orig (w',h') =
   box ((p1 * thumbScale, p2 * thumbScale), (w * thumbScale, h * thumbScale))
 where ((p1,p2),(w,h)) = zoomBoxDescription scale orig sz
       sz              = (fromIntegral w', fromIntegral h')

-- | Builds the box description for the zoom box from the scale, origin and container size
zoomBoxDescription :: Float -> Point -> (Float, Float) -> BoxDescription
zoomBoxDescription s (p1, p2) (w, h) = ((p1'/s, p2'/s), (w / s, h / s))
 where p1' = - (w / 2 + p1)
       p2' = - (h / 2 + p2)

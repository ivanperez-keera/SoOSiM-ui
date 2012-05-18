{-# LANGUAGE PackageImports  #-}
-- | Presents the SimState to the user and updates it with input events
module View.InitThumbnail where

-- External imports
import             Data.CBMVar
import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Animate
import             Graphics.UI.Gtk (ContainerClass, widgetGetSize)

-- Local imports
import Config.Config
import Config.Preferences
import Data.Tuple4
import View.Animation

-- Local imports: basic types
import Graphics.Diagrams.Types ( BoxDescription )
import Graphics.Gloss.AdvancedShapes.Boxes

-- | Draws a thumbnail of the main animation
drawThumb :: (ContainerClass a, ContainerClass b) => Config -> SimGlVar -> a -> b -> IO()
drawThumb cfg mcs e be =
  animateIO (InWidget e initialThumbnailSize)
    white (makeThumbnail cfg (widgetGetSize be) mcs)

-- | Convert our state to a smaller thumbnail 
makeThumbnail :: Config -> IO (Int, Int) -> SimGlVar -> a -> IO Picture
makeThumbnail cfg getSz st _ = do
  pcs <- makeImage cfg st thumbScale thumbCoords
  st' <- readCBMVar st
  sz  <- getSz
  return $ Pictures [ pcs
                    , translate thumbX thumbY $ paintZoomBox (trd4 st') sz 
                    ]

-- Paints the zoom box for a given scale, origin and container size
paintZoomBox :: (Float, Point) -> (Int, Int) -> Picture
paintZoomBox o (w',h') =
   box ((p1 * thumbScale, p2 * thumbScale), (w * thumbScale, h * thumbScale))
 where ((p1,p2),(w,h)) = zoomBoxDescription o sz
       sz              = (fromIntegral w', fromIntegral h')

-- | Builds the box description for the zoom box from the scale, origin and container size
zoomBoxDescription :: (Float, Point) -> (Float, Float) -> BoxDescription
zoomBoxDescription (s, (p1, p2)) (w, h) = ((p1'/s, p2'/s), (w / s, h / s))
 where p1' = - (w / 2 + p1)
       p2' = - (h / 2 + p2)
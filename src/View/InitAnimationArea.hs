{-# LANGUAGE PackageImports #-}
module View.InitAnimationArea where

-- External imports
import "gloss-gtk" Graphics.Gloss
import Graphics.UI.Gtk hiding (Color)

-- Local imports
import View.Objects

initialiseAnimationArea :: Builder -> IO ()
initialiseAnimationArea bldr = do
    drawPic =<< viewport1 bldr

drawPic w  = animate (InWidget w (800, 600))
                  white frame

frame time
        = Scale 0.8 0.8
        $ Rotate (time * 30)
        $ mach time 6
        
mach t 0 = leaf
mach t d
 = Pictures
        [ leaf
        , Translate 0 (-100) 
                $ Scale 0.8 0.8 
                $ Rotate (90 + t * 30) 
                $ mach (t * 1.5) (d - 1)

        , Translate 0   100 
                $ Scale 0.8 0.8 
                $ Rotate (90 - t * 30) 
                $ mach (t * 1.5) (d - 1) ]
        
leaf    = Pictures
                [ Color (makeColor 1.0 1.0 1.0 0.5) $ Polygon loop
                , Color (makeColor 0.0 0.0 1.0 0.8) $ Line loop ]

loop    = [(-10, -100), (-10, 100), (10, 100), (10, -100), (-10, -100)]

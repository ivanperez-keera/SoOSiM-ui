{-# LANGUAGE PackageImports #-}
module Graphics.GlossShapes where

import "gloss-gtk" Graphics.Gloss

-- Local imports
import Graphics.Types hiding (Color, makeColor)

labelledBox :: String -> BoxDescription -> Color -> Picture
labelledBox n ((x,y),(w,h)) c = shadowed $
 translate x y $ Pictures $
  [ boxBG d c
  , niceBox d
  , plusBox ((0,h - 20),(20,20))
  , translate xc yc $ scale 0.1 0.1 $ text n
  ]
 where tw  = 0.1 * fromIntegral (40 * length n) -- semi-fixed width :(
       w'  = max w tw
       d   = ((0,0),(20 + w',h))
       xc  = (w' - tw) / 2
       yc  = h - 40

boxBG :: BoxDescription -> Color -> Picture
boxBG bd c = color c $ polygon $ box bd

niceBox :: BoxDescription -> Picture
niceBox = line . box

shadowed :: Picture -> Picture
shadowed p = Pictures
  [ color (makeColor 0.8 0.8 0.8 1) $ translate 2 (-2) $ p
  , color (makeColor 0.8 0.8 0.8 1) $ translate 1 (-1) $ p
  , p
  ]

box :: BoxDescription -> Vertices
box ((x,y),(w,h)) = [ (x,y), (x+w,y), (x+w, y+h), (x, y+h), (x,y)]

plusBox :: BoxDescription -> Picture
plusBox bd@((x,y),(w,h)) = Pictures
 [ niceBox bd
 , line [(x + w2, y + m), (x + w2,    y + h - m)]
 , line [(x + m, y + h2), (x + w - m, y + h2)]
 ]
 where w2 = w / 2
       h2 = h / 2
       m  = h / 10

arrow :: Path -> Picture
arrow = line

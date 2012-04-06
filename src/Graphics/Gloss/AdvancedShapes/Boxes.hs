{-# LANGUAGE PackageImports #-}
-- | Several kinds of Gloss boxes
module Graphics.Gloss.AdvancedShapes.Boxes
   (box, fullBox, labelledBox, plusBox, minusBox)
  where

import "gloss-gtk" Graphics.Gloss

-- Local imports
import Graphics.Diagrams.Types hiding (Color, makeColor)
import Graphics.Gloss.AdvancedShapes.Shadows

-- | A simple box
box :: BoxDescription -> Picture
box = line . boxCorners

-- | A box with a full background
fullBox :: BoxDescription -> Picture
fullBox = polygon . boxCorners

-- | A nice box with text and, optionally, an expand/collapse menu icon
labelledBox :: String -> Maybe Bool -> BoxDescription -> Color -> Picture
labelledBox n expandable ((x,y),(w,h)) c = shadowed $
 translate x y $ Pictures
  [ color c $ fullBox d
  , box d
  , sign
  , translate xc yc $ scale 0.1 0.1 $ text n
  ]
 where tw  = 0.1 * fromIntegral (40 * length n) -- semi-fixed width :(
       w'  = max w tw
       d   = ((0,0),(20 + w',h))
       xc  = (w' - tw) / 2
       yc  = h - 40
       sign = case expandable of
               Nothing    -> blank
               Just True  -> minusBox ((0,h - 20),(20,20))
               Just False -> plusBox ((0,h - 20),(20,20))

-- | The corners of a box (given an origin and the dimensions)
boxCorners :: BoxDescription -> Path
boxCorners ((x,y),(w,h)) = [ (x,y), (x+w,y), (x+w, y+h), (x, y+h), (x,y)]

-- | A box with a plus sign in it
plusBox :: BoxDescription -> Picture
plusBox bd@((x,y),(w,h)) = Pictures
 [ box bd
 , line [(x + w2, y + m), (x + w2,    y + h - m)]
 , line [(x + m, y + h2), (x + w - m, y + h2)]
 ]
 where w2 = w / 2
       h2 = h / 2
       m  = h / 10

-- | A box with a minus sign in it
minusBox :: BoxDescription -> Picture
minusBox bd@((x,y),(w,h)) = Pictures
 [ box bd
 , line [(x + m, y + h2), (x + w - m, y + h2)]
 ]
 where h2 = h / 2
       m  = h / 10
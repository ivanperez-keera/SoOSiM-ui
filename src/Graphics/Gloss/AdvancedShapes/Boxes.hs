{-# LANGUAGE PackageImports #-}
-- | Several kinds of Gloss boxes
module Graphics.Gloss.AdvancedShapes.Boxes
   (box, fullBox, labelledBox, plusBox, minusBox, roundBox, semiRoundBox, roundBoxSolid, semiRoundBoxSolid)
  where

import Data.Maybe
import "gloss-gtk" Graphics.Gloss

-- Local imports
import Graphics.Diagrams.Types hiding (Color, makeColor)
import Graphics.Diagrams.Positioned.PositionedDiagram
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
  [ drawBox
  , translate xc yc $ scale 0.11 0.11 $ text n
  ]
 where tw  = fontWidth * fromIntegral (length n)
       w'  = (max w tw) + 2 * boxPadding
       -- nw  = w' + (if ex then 20 else 0)
       d   = ((0,0),(w,h))
       xc  = if ex then (w' - tw) / 2 else boxPadding
       yc  = h - 40
       ex  = isJust expandable
       menuBoxPos = (0, h - snd stdMenuBoxSize)
       sign = case expandable of
               Nothing    -> blank
               Just True  -> minusBox (menuBoxPos,stdMenuBoxSize)
               Just False -> plusBox  (menuBoxPos,stdMenuBoxSize)
       drawBox = case expandable of
                  Nothing -> Pictures [ color c $ roundBoxSolid d 10
                                      , roundBox d 10
                                      ]
                  _       -> Pictures [ color c $ semiRoundBoxSolid d 10
                                      , semiRoundBox d 10
                                      , sign
                                      ]

-- | Draws a solid box with round corners, except for the top-left corner
semiRoundBoxSolid :: BoxDescription -> Float -> Picture
semiRoundBoxSolid ((x,y),(w,h)) d =
   Pictures $ polygon ls' : arcs
  where ls'  = [ (x + d, y), (x + w - d, y), (x + w - d, y+d)
               , (x + w, y + d), (x+w, y + h - d), (x + w -d, y + h -d)
               , (x + w - d, y + h), (x, y + h)
               , (x, y + d), (x + d, y + d), (x + d, y)
               ]
        arcs = [ translate (x + d)     (y + d)     $ myArcSolid 180 270 d
               , translate (x + w - d) (y + d)     $ myArcSolid 270 360 d
               , translate (x + w - d) (y + h - d) $ myArcSolid 0   90  d
               -- , translate (x + d)     (y + h - d) $ arcSolid 90  180 d
               ]

-- | Draws a box with round corners, except for the top-left corner
semiRoundBox :: BoxDescription -> Float -> Picture
semiRoundBox ((x,y),(w,h)) d =
   Pictures $ map line ls' ++ arcs
  where ls'  = [ [ (x + d, y),         (x + w - d, y)   ]
               , [ (x + w, y + d),     (x+w, y + h - d) ]
               , [ (x + w - d, y + h), (x, y + h)       ]
               , [ (x, y + h),         (x, y + d)       ]
               ]
        arcs = [ translate (x + d)     (y + d)     $ arc 180 270 d
               , translate (x + w - d) (y + d)     $ arc 270 360 d
               , translate (x + w - d) (y + h - d) $ arc 0   90  d
               -- , translate (x + d)     (y + h - d) $ arc 90  180 d
               ]

-- | Draws a solid box with round corners
roundBoxSolid :: BoxDescription -> Float -> Picture
roundBoxSolid ((x,y),(w,h)) d =
   Pictures $ polygon ls' : arcs
  where ls'  = [ (x + d, y), (x + w - d, y), (x + w - d, y+d)
               , (x + w, y + d), (x+w, y + h - d), (x + w -d, y + h -d)
               , (x + w - d, y + h), (x+d, y + h), (x+d,y+h-d), (x, y+h-d)
               , (x, y + d), (x + d, y + d), (x + d, y)
               ]
        arcs = [ translate (x + d)     (y + d)     $ myArcSolid 180 270 d
               , translate (x + w - d) (y + d)     $ myArcSolid 270 360 d
               , translate (x + w - d) (y + h - d) $ myArcSolid 0   90  d
               , translate (x + d)     (y + h - d) $ myArcSolid 90  180 d
               ]

-- | Draws a box with round corners
roundBox :: BoxDescription -> Float -> Picture
roundBox ((x,y),(w,h)) d =
   Pictures $ map line ls' ++ arcs
  where ls'  = [ [ (x + d, y),         (x + w - d, y)   ]
               , [ (x + w, y + d),     (x+w, y + h - d) ]
               , [ (x + w - d, y + h), (x+d, y + h)     ]
               , [ (x, y + h-d),       (x, y + d)       ]
               ]
        arcs = [ translate (x + d)     (y + d)     $ arc 180 270 d
               , translate (x + w - d) (y + d)     $ arc 270 360 d
               , translate (x + w - d) (y + h - d) $ arc 0   90  d
               , translate (x + d)     (y + h - d) $ arc 90  180 d
               ]

-- | A better arcSolid (this one is actually solid)
myArcSolid :: Float -> Float -> Float -> Picture
myArcSolid a1 a2 r = polygon $
  [orig, p1] ++ ls ++ [p2, orig]
 where orig = (0,0)
       p1   = toP $ toA a1
       p2   = toP $ toA a2
       ls   = [ toP $ toA x | x <- [a1..a2]]
       toP x = (cos x * r, sin x * r)
       toA x = x * pi * 2 / 360

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

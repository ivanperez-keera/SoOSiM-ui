-- | Simple diagrams (boxes and arrows) with colours.
-- Every element has a position, meaning that
-- arrows are not given as connectors between boxes
-- but between points.
module Graphics.Diagrams.Positioned.PositionedDiagram
  ( PositionedDiagram(..) , PBox(..) , PArrow(..)
  , boxSep, boxPadding, fontWidth, fontHeight, pboxPos , pboxLimits, stdMenuBoxSize
  , pboxSize , pboxListSize
  )
 where

import Graphics.Diagrams.Types

-- | A diagram is just a collection of boxes and arrows
data PositionedDiagram = PositionedDiagram [PBox] [PArrow]
 deriving Show

-- | A box can be just a simple box, or a box with other boxes inside. The
-- positions of the boxes inside a box are relative to the (parent) box.
data PBox = PBox Name Name Position Size Color
          | PGroupBox Name Position Size [PBox] Color Bool
 deriving Show

-- | An arrow is just a line between two positions
data PArrow = PArrow Position Position
 deriving Show

-- | Standard box separation
boxSep :: Float
boxSep = 15

-- | Standard box padding
boxPadding :: Float
boxPadding = 5

fontWidth :: Float
fontWidth = 15

fontHeight :: Float
fontHeight = 70

stdMenuBoxSize :: (Float, Float)
stdMenuBoxSize = (20, 20)

-- | Returns the bottom-left and upper-right corner of a box
pboxLimits :: PBox -> (Position, Position)
pboxLimits b = ((boxMinX, boxMinY), (boxMaxX, boxMaxY))
  where (boxMinX, boxMinY) = pboxPos b
        (sizeX, sizeY)     = pboxSize b
        (boxMaxX, boxMaxY) = (boxMinX + sizeX, boxMinY + sizeY)

-- | Returns the position of a box
pboxPos :: PBox -> Position
pboxPos (PBox _ _ p _ _)          = p
pboxPos (PGroupBox _ p _ _ _ _) = p

-- | Returns the size of a box
pboxSize :: PBox -> Size
pboxSize (PBox _ _ _ s _)          = s
pboxSize (PGroupBox _ _ s _ _ _) = s

-- | Calculates the size of the minimal area that encloses a list of boxes
pboxListSize :: [PBox] -> Size
pboxListSize [] = (0,0)
pboxListSize (b:bs) = pboxListSize' (pboxLimits b) bs

pboxListSize' :: (Position, Position) -> [PBox] -> Size
pboxListSize' ((minX, minY), (maxX, maxY)) []     = (maxX - minX, maxY - minY)
pboxListSize' ((minX, minY), (maxX, maxY)) (b:bs) =
   pboxListSize' (newMins, newMaxs) bs
  where ((boxMinX, boxMinY) ,(boxMaxX, boxMaxY)) = pboxLimits b
        newMins = (min minX boxMinX, min minY boxMinY)
        newMaxs = (max maxX boxMaxX, max maxY boxMaxY)

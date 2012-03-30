module Graphics.PlainDiagram where

import Graphics.Types

data PlainDiagram = PlainDiagram [PBox] [PArrow]
 deriving Show
data PBox = PBox Name Position Size Color
          | PGroupBox Name Position Size [PBox] Color
 deriving Show
data PArrow = PArrow Position Position
 deriving Show

boxSep :: Float
boxSep = 15

pboxLimits :: PBox -> (Position, Position)
pboxLimits b = ((boxMinX, boxMinY), (boxMaxX, boxMaxY))
  where (boxMinX, boxMinY) = pboxPos b
        (sizeX, sizeY)     = pboxSize b
        (boxMaxX, boxMaxY) = (boxMinX + sizeX, boxMinY + sizeY)

pboxSize :: PBox -> Size
pboxSize (PBox _ _ s _)        = s
pboxSize (PGroupBox _ _ s _ _) = s

pboxPos :: PBox -> Position
pboxPos (PBox _ p _ _)        = p
pboxPos (PGroupBox _ p _ _ _) = p

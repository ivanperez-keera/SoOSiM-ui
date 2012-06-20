-- | Simple diagrams (boxes and arrows) with colours.
-- Every element has a position, meaning that
-- arrows are not given as connectors between boxes
-- but between points.
module Graphics.Diagrams.Positioned.PositionedDiagram
  ( PositionedDiagram(..) , PBox(..) , PArrow(..)
  , boxSep, boxPadding, fontWidth, fontHeight, pboxLimits, stdMenuBoxSize
  , pboxListSize , pboxKind, pboxSubBoxes, pboxExpanded
  , anyArea, anyMenu
  )
 where

import Data.Maybe

import Graphics.Diagrams.Types

-- | A diagram is just a collection of boxes and arrows
data PositionedDiagram = PositionedDiagram [PBox] [PArrow]
 deriving Show

-- | A box can be just a simple box, or a box with other boxes inside. The
-- positions of the boxes inside a box are relative to the (parent) box.
data PBox = PBox { pboxName     :: Name
                 , pboxKind_    :: Name
                 , pboxPosition :: Position
                 , pboxSize     :: Size
                 , pboxColor    :: Color
                 }
          | PGroupBox { pboxName      :: Name
                      , pboxPosition  :: Position
                      , pboxSize      :: Size
                      , pboxSubBoxes_ :: [PBox]
                      , pboxColor     :: Color
                      , pboxExpanded_ :: Bool
                      }
 deriving Show

-- | Returns the box kind, if any
pboxKind :: PBox -> Maybe Name
pboxKind b@(PBox {}) = Just $ pboxKind_ b
pboxKind _           = Nothing

-- | Returns the list of subboxes of this box (empty list if it's not a group
-- box)
pboxSubBoxes :: PBox -> [PBox]
pboxSubBoxes (PBox {}) = []
pboxSubBoxes b         = pboxSubBoxes_ b

-- | Returns whether the box is expanded or collapsed (always true for
-- simple boxes
pboxExpanded :: PBox -> Bool
pboxExpanded (PBox {}) = True
pboxExpanded b         = pboxExpanded_ b

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
  where (boxMinX, boxMinY) = pboxPosition b
        (sizeX, sizeY)     = pboxSize b
        (boxMaxX, boxMaxY) = (boxMinX + sizeX, boxMinY + sizeY)

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


-- | Returns the qualified name of the box who's menu
-- icon is in the given position (if any)
isMenuOfB :: Position -> PBox -> Maybe [Name]
isMenuOfB _  (PBox _ __ _ _ _) = Nothing
isMenuOfB p1 (PGroupBox n p2 s bs _ _)
 | isMenuOf p1 (p2,s) = Just [n]
 | otherwise          = fmap (n:) $ listToMaybe l
 where l   = mapMaybe (isMenuOfB p1') bs
       p1' = subPos p1 p2  -- p1 relative to p2

-- | Returns True if the given position is in the menu
-- icon area of a box with the given dimensions
isMenuOf :: Position -> (Position, Size) -> Bool
isMenuOf p1 (p2, (_,th)) = inArea p1 (p2', stdMenuBoxSize)
 where p2' = addPos p2 (0, th - (snd stdMenuBoxSize))

-- | Returns the qualified name of the box in the given position
isAreaOfB :: Position -> PBox -> Maybe [Name]
isAreaOfB p1 b
 | not (null l)         = fmap (n:) $ listToMaybe l
 | isAreaOf p1 (p2, sz) = Just [n]
 | otherwise            = Nothing
 where l   = mapMaybe (isAreaOfB p1') $ pboxSubBoxes b
       p1' = subPos p1 p2   -- p1 relative to p2
       n   = pboxName     b
       p2  = pboxPosition b
       sz  = pboxSize     b

-- | Returns True if the given position is in the
-- area of a box with the given dimensions
isAreaOf :: Position -> (Position, Size) -> Bool
isAreaOf p d = inArea p d && not (isMenuOf p d)

anyMenu :: Position -> PositionedDiagram -> Maybe [Name]
anyMenu p (PositionedDiagram boxes _) = listToMaybe $ mapMaybe (isMenuOfB p) boxes

anyArea :: Position -> PositionedDiagram -> Maybe [Name]
anyArea p (PositionedDiagram boxes _) = listToMaybe $ mapMaybe (isAreaOfB p) boxes  

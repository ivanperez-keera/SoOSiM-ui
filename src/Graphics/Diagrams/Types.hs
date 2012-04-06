{-# LANGUAGE PackageImports #-}

-- | Basic 2D coordinate manipulation and other aux
-- types and functions used in diagrams
module Graphics.Diagrams.Types
   ( 
   -- * Positions
     Position
   , addPos
   , subPos
   , multPos
   -- * Sizes
   , Size
   -- * Areas (boxes)
   , BoxDescription
   -- * Identifiers
   , Name
   -- * Alignment
   , VAlign(..)
   , HAlign(..)
   -- * Colors
   , Color
   , makeColor
   )
  where

import qualified "gloss-gtk" Graphics.Gloss as G

-- | An element's name
type Name = String

-- | The description of a box (vertex, size)
type BoxDescription = ((Float, Float), (Float, Float))

-- | A size as a pair of dimensions
type Size = (Float, Float)

-- | A position in a diagram
type Position = (Float, Float)

-- | Vertical alignment
data VAlign = VTop
            | VCenter
            | VBottom

-- | Horizontal alignment
data HAlign = HLeft
            | HCenter
            | HRight  

-- | Colours
type Color = G.Color

makeColor :: Float -> Float -> Float -> Float -> Color
makeColor = G.makeColor

-- | Adds two positions
addPos :: Position -> Position -> Position
addPos (p11,p12) (p21, p22) = (p11+p21, p12+p22)

-- | Substracts a position from another
subPos :: Position -> Position -> Position
subPos (p11,p12) (p21, p22) = (p11-p21, p12-p22)

-- | Multiplies two positions
multPos :: Position -> Position -> Position
multPos (p11,p12) (p21, p22) = (p11*p21, p12*p22)

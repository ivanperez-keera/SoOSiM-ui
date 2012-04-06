{-# LANGUAGE PackageImports #-}
-- | Transforms a Diagram with positions and sizes into a gloss picture
module Graphics.Diagrams.Transformations.PositionedDiagram2Picture
  (paintDiagram)
 where

-- External imports
import "gloss-gtk" Graphics.Gloss

-- Local imports
import Graphics.Diagrams.Positioned.PositionedDiagram
import Graphics.Gloss.AdvancedShapes

-- | Transforms a plain diagram into a gloss picture
-- by transforming the arrows and the boxes
paintDiagram :: PositionedDiagram -> Picture
paintDiagram (PositionedDiagram bs as) = Pictures [pbs, pas]
  where pbs = Pictures $ map paintBox bs
        pas = Pictures $ map paintArrow as

-- | Transforms a Plain Box into a Gloss Picture
paintBox :: PBox -> Picture
paintBox (PBox n position size c)           = labelledBox n Nothing (position, size) c
paintBox (PGroupBox n position size bs c e) = Pictures [b', bs']
  where b'  = labelledBox n (Just e) (position, size) c
        bs' = uncurry translate position $ Pictures $ map paintBox bs

-- | Transforms a Plain Arrow into a Gloss Picture
paintArrow :: PArrow -> Picture
paintArrow (PArrow p1 p2) = arrow [p1, p2]

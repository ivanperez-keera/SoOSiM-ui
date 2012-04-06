{-# LANGUAGE PackageImports #-}
-- | Arrow in gloss
module Graphics.Gloss.AdvancedShapes.Arrows where

import "gloss-gtk" Graphics.Gloss

-- | Plain arrow with no head
arrow :: Path -> Picture
arrow = line

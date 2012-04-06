{-# LANGUAGE PackageImports #-}
-- | Gloss Shadows
module Graphics.Gloss.AdvancedShapes.Shadows where

import "gloss-gtk" Graphics.Gloss

-- | Adds a dark shadow to a picture
shadowed :: Picture -> Picture
shadowed p = Pictures
  [ color (makeColor 0.8 0.8 0.8 1) $ translate 2 (-2) p
  , color (makeColor 0.8 0.8 0.8 1) $ translate 1 (-1) p
  , p
  ]

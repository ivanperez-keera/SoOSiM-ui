{-# LANGUAGE PackageImports #-}
-- | Gloss Shadows
module Graphics.Gloss.AdvancedShapes.Shadows where

import "gloss-gtk" Graphics.Gloss

import Config.Preferences

-- | Adds a dark shadow to a picture
shadowed :: Picture -> Picture
shadowed p = Pictures
  [ color shadowColor $ Pictures
      [ translate 2 (-2) p, translate 1 (-1) p ]
  , p
  ]

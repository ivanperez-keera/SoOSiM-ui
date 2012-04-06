{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards  #-}
-- | Arrow in gloss
module Graphics.Gloss.AdvancedShapes.Arrows where

import "gloss-gtk" Graphics.Gloss
import Config.Preferences (arrowDegrees, arrowLength)

-- | Plain arrow with no head
arrow :: Path -> Picture
arrow p 
  | [p1, p2] <- p, p1 /= p2
  , (v1, v2) <- calcArrowHeadVertex (p1, p2)
  = Pictures [ line [p1, p2]
             , polygon [p2, v1, v2]
             ]
arrow _ = blank

-- | Taken from Keera Gale IDE
calcArrowHeadVertex :: (Point, Point) -> (Point, Point)
calcArrowHeadVertex ((start_x, start_y), (end_x, end_y)) = ((x1, y1), (x2, y2))
  where angle = atan2 (end_y - start_y) (end_x - start_x)
        x1 = end_x - arrowLength * cos(angle - arrowDegrees)
        y1 = end_y - arrowLength * sin(angle - arrowDegrees)
        x2 = end_x - arrowLength * cos(angle + arrowDegrees)
        y2 = end_y - arrowLength * sin(angle + arrowDegrees)

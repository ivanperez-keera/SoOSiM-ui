module Graphics.Zoom 
        (zoomWith)
       where

type Point = (Float, Float)

-- | Zooms in/out of a state with a specific zoom
zoomWith :: Float -> Point -> (Float, Point) -> (Float, Point)
zoomWith f (p1, p2) (sc, (o1,o2)) = ((sc * f), o')
 where p1' = p1 * (1 - f)
       p2' = p2 * (1 - f)
       o'  = (o1 * f + p1', o2 * f + p2')
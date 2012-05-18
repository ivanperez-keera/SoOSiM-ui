{-# LANGUAGE PackageImports #-}
module Config.Preferences where

import Config.Config
import "gloss-gtk" Graphics.Gloss
import Graphics.Diagrams.MultiCoreStatus

-- | Default angle of arrow heads (in pi-radians)
arrowDegrees :: Float
arrowDegrees = 0.3

-- | Default arrow length
arrowLength :: Float
arrowLength = 25

-- | Color used for shadows
shadowColor :: Color
shadowColor = makeColor 0.8 0.8 0.8 1

-- | Transforms the current processing unit status (selected) into a colour
processingUnitColor :: Config -> Bool -> Color
processingUnitColor ((x,_),_,_,_) True  = makeColorT x
processingUnitColor ((_,x),_,_,_) False = makeColorT x

-- | Transforms the current running element status (Selected, State) into a
-- colour
runningElementColor :: Config -> Bool -> ElementState -> Color
runningElementColor (_,(x,_),_,_) True  Active  = makeColorT x
runningElementColor (_,_,(x,_),_) True  Waiting = makeColorT x
runningElementColor (_,_,_,(x,_)) True  Idle    = makeColorT x
runningElementColor (_,(_,x),_,_) False Active  = makeColorT x
runningElementColor (_,_,(_,x),_) False Waiting = makeColorT x
runningElementColor (_,_,_,(_,x)) False Idle    = makeColorT x

makeColorT :: Color4 -> Color
makeColorT (r,g,b,a) = makeColor r g b a

type ViewState = (Float, Point)

-- | Initial zoom and position
initialViewState :: ViewState
initialViewState = (0.5, (-400, -100))

stdZoomStep :: Float
stdZoomStep = 0.8

initialAnimationSize :: (Int, Int)
initialAnimationSize = (800, 600)

initialThumbnailSize :: (Int, Int)
initialThumbnailSize = (200, 150)

-- | Default thumbnail zoom level
thumbScale :: Float
thumbScale = 0.05

-- | Thumbnail base coords
thumbCoords :: Point
thumbCoords = (thumbX, thumbY)

-- | Thumbnail base X coord
thumbX :: Float
thumbX = (-90)

-- | Thumbnail base Y coord
thumbY :: Float
thumbY = 0

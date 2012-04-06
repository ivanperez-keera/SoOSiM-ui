{-# LANGUAGE PackageImports #-}
module Config.Preferences where

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
processingUnitColor :: Bool -> Color
processingUnitColor True  = makeColor 0.1 0.5 0.9 1.0 
processingUnitColor False = makeColor 0.9 0.9 0.9 1.0

-- | Transforms the current running element status (Selected, State) into a
-- colour
runningElementColor :: Bool -> ElementState -> Color
runningElementColor True  Active  = makeColor 0.5 0.9 0.7 1.0
runningElementColor True  Waiting = makeColor 0.9 0.9 0.3 1.0
runningElementColor True  Idle    = makeColor 0.9 0.9 0.6 1.0
runningElementColor False Active  = makeColor 0.5 0.9 0.9 1.0
runningElementColor False Waiting = makeColor 0.9 0.9 0.5 1.0
runningElementColor False Idle    = makeColor 0.9 0.9 0.9 1.0

{-# LANGUAGE PackageImports #-}
module Config.Preferences where

import "gloss-gtk" Graphics.Gloss
import Graphics.Diagrams.MultiCoreStatus
import Config.Config

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

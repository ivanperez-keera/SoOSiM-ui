module Graphics.Diagram where

import Graphics.Types

data Diagram = Diagram [Box] [Arrow]
data Box = Box Name Color
         | GroupBox Name [Box] Color
data Arrow = Arrow [Name] [Name]
 deriving Show

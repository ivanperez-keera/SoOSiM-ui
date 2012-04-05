module Graphics.Diagram where

import Graphics.Types

data Diagram = Diagram [Box] [Arrow]
data Box = Box Name Color
         | GroupBox Name [Box] Color Expanded
data Arrow = Arrow [Name] [Name]
 deriving Show

type Expanded = Bool

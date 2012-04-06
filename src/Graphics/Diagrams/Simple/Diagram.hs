-- | Simple position-less diagrams made of boxes and arrows
module Graphics.Diagrams.Simple.Diagram where

import Graphics.Diagrams.Types

-- | A diagram is just a collection of boxes and arrows
data Diagram = Diagram [Box] [Arrow]

-- | A box can either be a simple box, with two labels and a colour, or a group
-- box, which has other boxes inside. A group box can be expanded or collapsed.
data Box = Box Name Name Color
         | GroupBox Name [Box] Color Expanded

-- | An arrow links two boxes using their qualified names
data Arrow = Arrow [Name] [Name]
 deriving Show

-- | Group box's expanded status
type Expanded = Bool

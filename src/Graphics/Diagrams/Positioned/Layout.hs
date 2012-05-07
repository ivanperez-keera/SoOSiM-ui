-- | A helper module to lay elements out in columns or rows 
module Graphics.Diagrams.Positioned.Layout
    ( pboxColumnLayout
    , pboxRowLayout
    )
   where

-- External imports
import Data.List (mapAccumL)

-- Internal imports
import Graphics.Diagrams.Types
import Graphics.Diagrams.Positioned.PositionedDiagram

-- | Arranges boxes in column
pboxColumnLayout :: Float  -- ^ Initial top or base position
                 -> Float  -- ^ Minimum column width
                 -> HAlign -- ^ Horizontal alignment
                 -> [PBox] -- ^ Boxes to arrange in column
                 -> [PBox] -- ^ Arranged boxes
pboxColumnLayout base w align bs =
   snd $ mapAccumL (pboxColumnLayoutP boxSep width align) base bs
 where width = max w (fst (pboxListSize bs))

-- | Arranges one box in a column, returning the new top and the modified box
pboxColumnLayoutP :: Float -> Float -> HAlign -> Float -> PBox -> (Float, PBox)
pboxColumnLayoutP left maxWidth align top box = (top', box')
 where top' = top + boxSep + boxH
       box' = box { pboxPosition = (left + newX, top) }
       (boxW,boxH) = pboxSize box

       -- Align
       newX = case align of
                HLeft   -> 0
                HCenter -> (maxWidth - boxW) / 2
                HRight  -> maxWidth - boxW

-- | Arranges boxes in a row
pboxRowLayout :: VAlign -- ^ Vertical alignment
              -> [PBox] -- ^ List of boxes
              -> [PBox] -- ^ Arranged boxes
pboxRowLayout align bs = snd $ mapAccumL (pboxRowLayoutP height align) 0 bs
 where height = snd $ pboxListSize bs

-- | Arranges one box in a row, returning the new left and the modified box
pboxRowLayoutP :: Float -> VAlign -> Float -> PBox -> (Float, PBox)
pboxRowLayoutP maxHeight align left box = (left', box')
  where left' = left + boxSep + 40 + boxW
        box'  = box { pboxPosition = (left, newY) }
        (boxW,boxH) = pboxSize box
        
        -- Align
        newY = case align of
                 VTop    -> maxHeight - boxH
                 VCenter -> (maxHeight - boxH) / 2
                 VBottom -> 0 -- maxHeight - boxH

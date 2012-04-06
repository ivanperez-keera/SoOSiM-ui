{-# LANGUAGE PatternGuards  #-}
-- | Transforms a diagram with no positions into a diagram with
-- positions
module Graphics.Diagrams.Transformations.Diagram2PositionedDiagram
   ( transformDiagram )
  where

import Data.Maybe

import Graphics.Diagrams.Types
import Graphics.Diagrams.Simple.Diagram
import Graphics.Diagrams.Positioned.Layout
import Graphics.Diagrams.Positioned.PositionedDiagram


-- | Transforms a diagram into a diagram with positions and
-- sizes for all elements
--
-- FIXME: This is a lousy name
transformDiagram :: Diagram -> PositionedDiagram
transformDiagram (Diagram boxes arrows) = PositionedDiagram pboxes parrows
 where pboxes  = pboxRowLayout VTop $ map transformBox boxes
       parrows = mapMaybe (arrowToPArrow pboxes) arrows

-- | Transforms a box into a box with a position and a size
transformBox :: Box -> PBox
transformBox (Box s k c)        = PBox s k (0,0) (fromIntegral $ 40 * l, 70) c
 where l = length (s ++ " : " ++ k)
transformBox (GroupBox s g c e) = PGroupBox s (0,0) (wM,hP) g'' c e
 where g'      = map transformBox g
       g''     = pboxColumnLayout boxSep w HCenter g'
       (w,h)   = (fromIntegral $ 40 * length s, 70)
       (w',h') = pboxListSize g''
       wM      = max w' w + 2 * boxSep
       hP      = h + h' + boxSep 

-- | Transforms an arrow between names into an arrow between positions
arrowToPArrow :: [PBox] -> Arrow -> Maybe PArrow
arrowToPArrow bs (Arrow n1 n2) = do
  p1 <- findPosition n1 bs
  p2 <- findPosition n2 bs
  return $ PArrow p1 p2

-- | Finds the position of a box by its name
findPosition :: [Name] -> [PBox] -> Maybe Position
findPosition [] _  = Nothing
findPosition _  [] = Nothing
findPosition n1 ((PBox n2 _ p _ _):bs) 
  | (n11:_) <- n1 , n11 == n2 = Just p
  | otherwise                 = findPosition n1 bs
findPosition n1 ((PGroupBox n2 p _ bs _ _):bss)
 | n1 == [n2]                = Just p
 | head n1 == n2 && isJust l = fmap (addPos p) l
 | head n1 == n2             = Nothing
 | otherwise                 = findPosition n1 bss
 where l = listToMaybe $ mapMaybe (findPosition (tail n1).(:[])) bs

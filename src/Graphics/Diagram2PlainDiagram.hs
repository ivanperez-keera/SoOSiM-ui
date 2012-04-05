{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards  #-}
module Graphics.Diagram2PlainDiagram where

import Data.Maybe

import Graphics.Types
import Graphics.Diagram
import Graphics.PlainDiagram
import Graphics.PlainDiagramLayout

transformDiagram :: Diagram -> PlainDiagram
transformDiagram (Diagram boxes arrows) = PlainDiagram pboxes parrows
 where pboxes  = pboxRowLayout VTop $ map transformBox boxes
       parrows = mapMaybe (arrowToPArrow pboxes) arrows

transformBox :: Box -> PBox
transformBox (Box s c)        = PBox s (0,0) (fromIntegral $ 40 * length s, 70) c
transformBox (GroupBox s g c) = PGroupBox s (0,0) (wM,hP) g'' c
 where g'      = map transformBox g
       g''     = pboxColumnLayout boxSep w HCenter g'
       (w,h)   = (fromIntegral $ 40 * length s, 70)
       (w',h') = pboxListSize g''
       wM      = (max w' w) + 2 * boxSep
       hP      = h + h' + boxSep 

arrowToPArrow :: [PBox] -> Arrow -> Maybe PArrow
arrowToPArrow bs (Arrow n1 n2) = do
  p1 <- findPosition n1 bs
  p2 <- findPosition n2 bs
  return $ PArrow p1 p2

findPosition :: [Name] -> [PBox] -> Maybe Position
findPosition [] _  = Nothing
findPosition _  [] = Nothing
findPosition n1 ((PBox n2 p _ _):bs) 
  | (n11:_) <- n1 , n11 == n2 = Just p
  | otherwise                 = findPosition n1 bs
findPosition n1 ((PGroupBox n2 p _ bs _):bss)
 | n1 == [n2]                = Just p
 | head n1 == n2 && isJust l = fmap (addPos p) l
 | head n1 == n2             = Nothing
 | otherwise                 = findPosition n1 bss
 where l = listToMaybe $ mapMaybe (findPosition (tail n1).(:[])) bs

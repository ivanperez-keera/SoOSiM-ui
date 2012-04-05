{-# LANGUAGE PackageImports #-}
module Graphics.PlainDiagramLayout where

import Graphics.Types
import Graphics.PlainDiagram

pboxColumnLayout :: Float -> Float -> HAlign -> [PBox] -> [PBox]
pboxColumnLayout base w align bs = pboxColumnLayout' base boxSep width align bs
 where width = max w (fst (pboxListSize bs))

pboxColumnLayout' :: Float -> Float -> Float -> HAlign -> [PBox] -> [PBox]
pboxColumnLayout' _left _top _maxWidth _align []     = []
pboxColumnLayout' left  top  maxWidth  align (b:bs) = b':bs'
 where bs' = pboxColumnLayout' left top' maxWidth align bs
       sz  = pboxSize b
       b'  = case b of
               PBox n _ sz@(w,_) c           -> PBox n ((left + newX w),top) sz c
               PGroupBox n _ sz@(w,_) gs c e -> PGroupBox n ((left + newX w),top) sz gs c e
       top' = top + boxSep + snd sz
       newX w = case align of
                 HLeft   -> 0
                 HCenter -> (maxWidth - w) / 2
                 HRight  -> maxWidth - w

pboxRowLayout :: VAlign -> [PBox] -> [PBox]
pboxRowLayout align bs = pboxRowLayout' 0 height align bs
 where height = snd $ pboxListSize bs

pboxRowLayout' :: Float -> Float -> VAlign -> [PBox] -> [PBox]
pboxRowLayout' _left _maxHeight _align []     = []
pboxRowLayout' left  maxHeight  align  (b:bs) = b':bs'
  where bs' = pboxRowLayout' left' maxHeight align bs
        sz  = pboxSize b
        b'  = case b of
               PBox n _ sz@(_,h) c           -> PBox n (left,(newY h)) sz c
               PGroupBox n _ sz@(_,h) gs c e -> PGroupBox n (left,(newY h)) sz gs c e
        left' = left + boxSep + 40 + fst sz
        newY h = case align of
                  VTop    -> maxHeight - h
                  VCenter -> (maxHeight - h) / 2
                  VBottom -> maxHeight - h

pboxListSize :: [PBox] -> Size
pboxListSize [] = (0,0)
pboxListSize (b:bs) = pboxListSize' (pboxLimits b) bs

pboxListSize' :: (Position, Position) -> [PBox] -> Size
pboxListSize' ((minX, minY), (maxX, maxY)) []     = (maxX - minX, maxY - minY)
pboxListSize' ((minX, minY), (maxX, maxY)) (b:bs) =
   pboxListSize' (newMins, newMaxs) bs
  where ((boxMinX, boxMinY) ,(boxMaxX, boxMaxY)) = pboxLimits b
        newMins = (min minX boxMinX, min minY boxMinY)
        newMaxs = (max maxX boxMaxX, max maxY boxMaxY)

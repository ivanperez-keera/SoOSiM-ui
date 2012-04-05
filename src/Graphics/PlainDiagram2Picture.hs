{-# LANGUAGE PackageImports #-}
module Graphics.PlainDiagram2Picture where

-- External imports
import "gloss-gtk" Graphics.Gloss

-- Local imports
import Graphics.PlainDiagram
import Graphics.GlossShapes

paintDiagram :: PlainDiagram -> Picture
paintDiagram (PlainDiagram bs as) = Pictures [pbs, pas]
  where pbs = Pictures $ map paintBox bs
        pas = Pictures $ map paintArrow as

paintBox :: PBox -> Picture
paintBox (PBox n position size c)           = labelledBox n Nothing (position, size) c
paintBox (PGroupBox n position size bs c e) = Pictures [b', bs']
  where b'  = labelledBox n (Just e) (position, size) c
        bs' = translate (fst position) (snd position) $ Pictures $ map paintBox bs

paintArrow :: PArrow -> Picture
paintArrow (PArrow p1 p2) = arrow [p1, p2]

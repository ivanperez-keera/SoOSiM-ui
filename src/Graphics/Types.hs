{-# LANGUAGE PackageImports #-}
module Graphics.Types where

import "gloss-gtk" Graphics.Gloss as G

type BoxDescription = ((Float, Float), (Float, Float))

type Vertices = [ Point ]

type Size           = (Float, Float)
type WithSize a     = (a, Size)
type WithLocation a = (a, Point)
type WithId a b     = (a, b)

type Name = String
type Position = (Float, Float)

data VAlign = VTop
            | VCenter
            | VBottom

data HAlign = HLeft
            | HCenter
            | HRight  

type Color = G.Color

makeColor = G.makeColor

addPos :: Position -> Position -> Position
addPos (p11,p12) (p21, p22) = (p11+p21, p12+p22)

multPos :: Position -> Position -> Position
multPos (p11,p12) (p21, p22) = (p11*p21, p12*p22)

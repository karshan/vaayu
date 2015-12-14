#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Formats.STL
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)

upZ = (0, 0, 1)
downZ = (0, 0, -1)
upY = (0, 1, 0)
downY = (0, -1, 0)
leftX = (-1, 0, 0)
rightX = (1, 0, 0)

quad normal (a, b, c, d) =
    [ Triangle (Just normal) (a, b, c)
    , Triangle (Just normal) (a, c, d)
    ]

bottomLayer :: [Triangle]
bottomLayer = concat $
    [ quad upZ    ((1.0, 0.0, 0.0), (1.1, 0.0, 0.0), (1.1, 1.1, 0.0), (1.0, 1.0, 0.0))
    , quad downZ  ((1.0, 0.0, 1.0), (1.1, 0.0, 1.0), (1.1, 1.1, 1.0), (1.0, 1.0, 1.0))
    , quad upY    ((1.0, 0.0, 0.0), (1.1, 0.0, 0.0), (1.1, 0.0, 1.1), (1.0, 0.0, 1.0))
    , quad downY  ((1.0, 1.0, 0.0), (1.1, 1.0, 0.0), (1.1, 1.0, 1.1), (1.0, 1.0, 1.0))
    , quad rightX ((1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (1.0, 1.0, 1.0), (1.0, 0.0, 1.0))
    , quad leftX  ((1.1, 0.0, 0.0), (1.1, 1.1, 0.0), (1.1, 1.1, 1.1), (1.1, 0.0, 1.1))
    ]

ts :: [Triangle]
ts = bottomLayer

main = writeFile "out.stl" $ toLazyByteString $ textSTL $ STL "grid" ts

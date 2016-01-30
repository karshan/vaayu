#!/usr/bin/env runhaskell
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Formats.STL
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)

deriving instance Eq Triangle

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

cuboid l w h (ox, oy, oz) = concat $
    [ quad upZ    ((ox, oy, oz), (ox + l, oy, oz), (ox + l, oy + w, oz), (ox, oy + w, oz))
    , quad downZ  ((ox, oy, oz + h), (ox + l, oy, oz + h), (ox + l, oy + w, oz + h), (ox, oy + w, oz + h))
    , quad upY    ((ox, oy, oz), (ox + l, oy, oz), (ox + l, oy, oz + h), (ox, oy, oz + h))
    , quad downY  ((ox, oy + w, oz), (ox + l, oy + w, oz), (ox + l, oy + w, oz + h), (ox, oy + w, oz + h))
    , quad rightX ((ox, oy, oz), (ox, oy + w, oz), (ox, oy + w, oz + h), (ox, oy, oz + h))
    , quad leftX  ((ox + l, oy, oz), (ox + l, oy + w, oz), (ox + l, oy + w, oz + h), (ox + l, oy, oz + h))
    ]

bottomLayer :: [Triangle]
bottomLayer = concat $
    [ quad upZ    ((1.0, 0.0, 0.0), (1.1, 0.0, 0.0), (1.1, 1.0, 0.0), (1.0, 1.0, 0.0))
    , quad downZ  ((1.0, 0.0, 1.0), (1.1, 0.0, 1.0), (1.1, 1.0, 1.0), (1.0, 1.0, 1.0))
    , quad upY    ((1.0, 0.0, 0.0), (1.1, 0.0, 0.0), (1.1, 0.0, 1.0), (1.0, 0.0, 1.0))
    , quad downY  ((1.0, 1.0, 0.0), (1.1, 1.0, 0.0), (1.1, 1.0, 1.0), (1.0, 1.0, 1.0))
    , quad rightX ((1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (1.0, 1.0, 1.0), (1.0, 0.0, 1.0))
    , quad leftX  ((1.1, 0.0, 0.0), (1.1, 1.0, 0.0), (1.1, 1.0, 1.0), (1.1, 0.0, 1.0))
    ]

ts :: [Triangle]
ts = bottomLayer

main = writeFile "out.stl" $ toLazyByteString $ textSTL $ STL "grid" $ cuboid 0.5 0.5 0.5 (0,0,0)

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Alpaca.Geo.Dir (
      Dir
    , dirSin
    , dirCos
    , v2ToDir
    , radToDir
) where

import           Alpaca.Geo.Rad
import           Alpaca.Geo.V2

-- |A direction in 2D represented by the sin and cos of the angle
-- With the x axis.
data Dir = Dir {
        dirCos :: Double,
        dirSin :: Double
    } deriving (Eq)

v2ToDir :: V2 -> Maybe Dir
v2ToDir v = do
    V2 c s <- normalize v
    return (Dir c s)

radToDir :: Rad -> Dir
radToDir (Rad r) = Dir (cos r) (sin r)

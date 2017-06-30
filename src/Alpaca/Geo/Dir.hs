{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Alpaca.Geo.Dir (
      Dir
    , dirSin
    , dirCos
    , dirToV2
    , dirToRad
    , v2ToDir
    , radToDir
) where

import           Alpaca.Geo.Rad
import           Alpaca.Geo.V2

-- |A direction in 2D represented by the sin and cos of the angle
-- With the x axis.
newtype Dir = Dir { dirToV2 :: V2 }

instance HV2 Dir where
    {-# INLINE toV2 #-}
    toV2 = dirToV2

dirCos :: Dir -> Double
dirCos (Dir (V2 c _)) = c

dirSin :: Dir -> Double
dirSin (Dir (V2 _ s)) = s

dirToRad :: Dir -> Rad
dirToRad = Rad . acos . dirCos

v2ToDir :: V2 -> Maybe Dir
v2ToDir v = Dir <$> normalize v

radToDir :: Rad -> Dir
radToDir (Rad r) = Dir (V2 (cos r) (sin r))

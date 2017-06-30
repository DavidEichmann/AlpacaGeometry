module Alpaca.Geo (
      p2ToV2
    , v2ToP2
    , v2ToRad
    , dirToUnitV2
    , dirToRad
    , radToUnitV2
    , module Export
) where

import           Alpaca.Geo.Dir  as Export
import           Alpaca.Geo.Line as Export
import           Alpaca.Geo.P2   as Export
import           Alpaca.Geo.Rad  as Export
import           Alpaca.Geo.V2   as Export

-- P2 convertions

p2ToV2 :: P2 -> V2
p2ToV2 (P2 x y) = V2 x y

-- V2 convertions

v2ToP2 :: V2 -> P2
v2ToP2 (V2 x y) = P2 x y

v2ToRad :: V2 -> Maybe Rad
v2ToRad v@(V2 x y)
    | v == 0    = Nothing
    | otherwise = Just (Rad (atan2 y x))

-- Dir convertions

dirToUnitV2 :: Dir -> V2
dirToUnitV2 d = V2 (dirCos d) (dirSin d)

dirToRad :: Dir -> Rad
dirToRad = Rad . acos . dirCos

-- Rad convertions

radToUnitV2 :: Rad -> V2
radToUnitV2 (Rad r) = V2 (cos r) (sin r)

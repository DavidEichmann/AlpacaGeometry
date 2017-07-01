{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Alpaca.Geo (
      p2ToV2
    , v2ToP2
    , module Export
) where

import           Alpaca.Geo.Line as Export
import           Alpaca.Geo.P2   as Export
import           Alpaca.Geo.Rad  as Export
import           Alpaca.Geo.V2   as Export

-- P2 convertions

p2ToV2 :: P2 -> V2 'VAny
p2ToV2 (P2 x y) = V2 x y

-- V2 convertions

v2ToP2 :: V2 a -> P2
v2ToP2 (V2 x y) = P2 x y

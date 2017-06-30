{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.P2 (
      P2 (..)
) where

import           Alpaca.Geo.HMath
import           Alpaca.Geo.V2

{-# INLINE pmap #-}

-- |A point in 2D.
-- ⟦P2 x y⟧ = (x, y) ∈ R²
data P2 = P2 Double Double

instance Num P2 where
    (P2 x1 y1) + (P2 x2 y2) = P2 (x1 + x2) (y1  + y2)
    (P2 x1 y1) - (P2 x2 y2) = P2 (x1 - x2) (y1  - y2)
    (P2 x1 y1) * (P2 x2 y2) = P2 (x1 * x2) (y1  * y2)
    negate = pmap negate
    abs = pmap abs
    signum = pmap signum
    fromInteger i = P2 (fromInteger i) (fromInteger i)

pmap :: (Double -> Double) -> P2 -> P2
pmap f (P2 x y) = P2 (f x) (f y)

instance P2 :+ V2 where
    type P2 .+ V2 = P2
    (P2 px py) .+ (V2 vx vy) = P2 (px + vx) (py + vy)

instance P2 :- P2 where
    type P2 .- P2 = V2
    (P2 x1 y1) .- (P2 x2 y2) = V2 (x1 - x2) (y1 - y2)

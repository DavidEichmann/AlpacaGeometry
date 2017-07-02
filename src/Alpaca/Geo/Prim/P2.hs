{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Prim.P2 (
      P2 (..)
) where

import           Alpaca.HMath

{-# INLINE pmap #-}

-- |A point in 2D.
-- ⟦P2 x y⟧ = (x, y) ∈ R²
data P2 = P2 Double Double
    deriving (Eq, Ord)

instance P2 :+ P2 where
    type P2 .+ P2 = P2
    (P2 x1 y1) .+ (P2 x2 y2) = P2 (x1 + x2) (y1 + y2)

instance P2 :* Double where
    type P2 .* Double = P2
    (P2 x1 y1) .* s = P2 (x1 * s) (y1 * s)

instance Double :* P2 where
    type Double .* P2 = P2
    (.*) = flip (.*)

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

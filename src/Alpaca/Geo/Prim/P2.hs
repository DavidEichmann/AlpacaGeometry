{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Prim.P2 (
      P2 (..)
) where

{-# INLINE pmap #-}

-- |A point in 2D.
-- ⟦P2 x y⟧ = (x, y) ∈ R²
data P2 = P2 Double Double
    deriving (Eq)


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

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Alpaca.Geo.Rad (
      Rad(..)
) where

{-# INLINE rmap #-}

-- |A direction in 2D represented by the sin and cos of the angle
-- With the x axis.
newtype Rad = Rad Double

instance Num Rad where
    (Rad r1) + (Rad r2) = Rad (r1 + r2)
    (Rad r1) - (Rad r2) = Rad (r1 - r2)
    (Rad r1) * (Rad r2) = Rad (r1 * r2)
    negate = rmap negate
    abs = rmap abs
    signum = rmap signum
    fromInteger i = Rad (fromInteger i)

rmap :: (Double -> Double) -> Rad -> Rad
rmap f (Rad r) = Rad (f r)

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.V2 (
      V2 (..)
    , (⋅)
    , (×)
    , norm
    , normSq
    , normalize
) where

import           Alpaca.Geo.HMath

{-# INLINE (⋅) #-}
{-# INLINE (×) #-}
{-# INLINE norm #-}
{-# INLINE normSq #-}
{-# INLINE normalize #-}

-- |A vector in 2D.
data V2 = V2 Double Double
    deriving (Eq)

instance Num V2 where
    (V2 x1 y1) + (V2 x2 y2) = V2 (x1 + x2) (y1  + y2)
    (V2 x1 y1) - (V2 x2 y2) = V2 (x1 - x2) (y1  - y2)
    (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1  * y2)
    negate = vmap negate
    abs = vmap abs
    signum = vmap signum
    fromInteger i = V2 (fromInteger i) (fromInteger i)

vmap :: (Double -> Double) -> V2 -> V2
vmap f (V2 x y) = V2 (f x) (f y)

-- |Dot product.
(⋅) :: V2 -> V2 -> Double
(V2 x1 y1) ⋅ (V2 x2 y2) = (x1 * x2) + (y1 * y2)

-- |Cross product. This assumes a Z component of 0 for the inputs and returns
-- the z component of the cross product (the x and y components are always 0).
(×) :: V2 -> V2 -> Double
(V2 x1 y1) × (V2 x2 y2) = (x1 * y2) - (y1 * x2)

-- |Division by a scalar.
instance V2 :/ Double where
    {-# INLINE (./) #-}
    type V2 ./ Double = V2
    (V2 x y) ./ s = V2 (x / s) (y / s)

-- |Multiplication by a scalar.
instance V2 :* Double where
    {-# INLINE (.*) #-}
    type V2 .* Double = V2
    (V2 x y) .* s = V2 (x * s) (y * s)

-- |Multiplication by a scalar.
instance Double :* V2 where
    {-# INLINE (.*) #-}
    type Double .* V2 = V2
    (.*) = flip (.*)

-- |The norm (length) of a vector.
norm :: V2 -> Double
norm = sqrt . normSq

-- |The squared norm (length) of a vector.
normSq :: V2 -> Double
normSq v = v ⋅ v

-- |The normalized vector.
normalize :: V2 -> Maybe V2
normalize v
    | v == 0    = Nothing
    | otherwise = Just (v ./ norm v)

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.V2 (
      V2 (..)
    , VP (..)
    , NonZero
    , NonZeroB
    , Unit
    , UnitB
    , downgrade
    , (⋅)
    , (×)
    , norm
    , normSq
    , normalize
    , nonZero
    , v2Cos
    , v2Sin
    , v2ToRad
    , radToV2
) where

import           Alpaca.Geo.HMath
import           Alpaca.Geo.P2
import           Alpaca.Geo.Rad
import           Unsafe.Coerce

{-# INLINE (⋅) #-}
{-# INLINE (×) #-}
{-# INLINE norm #-}
{-# INLINE normSq #-}
{-# INLINE normalize #-}

-- |A vector in 2D. Parameter p describes the properties of the vector.
data V2 (p :: VP) where
    V2 :: Double -> Double -> V2 'VAny

deriving instance Eq (V2 a)

-- |Properties of a V2
data VP
    = VAny
    | VNonZero
    | VUnit

-- |Property relations.
class (~>) (a :: VP) (b :: VP)
instance (a ~>? b ~ 'True) => (~>) a b
type family (~>?) (a :: VP) (b :: VP) where
    'VAny     ~>? 'VAny      = 'True
    'VNonZero ~>? 'VAny      = 'True
    'VNonZero ~>? 'VNonZero  = 'True
    'VUnit    ~>? 'VAny      = 'True
    'VUnit    ~>? 'VNonZero  = 'True
    'VUnit    ~>? 'VUnit     = 'True
    a         ~>? b          = 'False

-- |NonZero property.
class NonZero (a :: VP)
instance (NonZeroB a ~ 'True) => NonZero a
type NonZeroB (p :: VP) = p ~>? 'VNonZero

-- |Unit length property.
class Unit (a :: VP)
instance (UnitB a ~ 'True) => Unit a
type UnitB (p :: VP) = p ~>? 'VUnit

-- |Vectors with different properties can still be equal
-- e.g. (V2 @'VAny 1 0) .== (V2 @'VUnit 1 0)
instance HEq (V2 a) (V2 b) where
    (V2 x1 y1) .== (V2 x2 y2) = x1 == x2 && y1 == y2

-- |Num instance for the 'VAny vector. Not all Num functions
-- maintain Non-Zero and Unit properties.
instance Num (V2 'VAny) where
    (V2 x1 y1) + (V2 x2 y2) = V2 (x1 + x2) (y1  + y2)
    (V2 x1 y1) - (V2 x2 y2) = V2 (x1 - x2) (y1  - y2)
    (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1  * y2)
    negate = vmap negate
    abs = vmap abs
    signum = vmap signum
    fromInteger i = V2 (fromInteger i) (fromInteger i)

vmap :: (Double -> Double) -> V2 'VAny -> V2 'VAny
vmap f (V2 x y) = V2 (f x) (f y)

-- |Dot product.
(⋅) :: V2 a -> V2 b -> Double
(V2 x1 y1) ⋅ (V2 x2 y2) = (x1 * x2) + (y1 * y2)

-- |Cross product. This assumes a Z component of 0 for the inputs and returns
-- the z component of the cross product (the x and y components are always 0).
(×) :: V2 a -> V2 b -> Double
(V2 x1 y1) × (V2 x2 y2) = (x1 * y2) - (y1 * x2)

-- |Division by a scalar.
instance V2 a :/ Double where
    {-# INLINE (./) #-}
    type V2 a ./ Double = V2 'VAny
    (V2 x y) ./ s = V2 (x / s) (y / s)

-- |Multiplication by a scalar.
instance V2 a :* Double where
    {-# INLINE (.*) #-}
    type V2 a .* Double = V2 'VAny
    (V2 x y) .* s = V2 (x * s) (y * s)

-- |Multiplication by a scalar.
instance Double :* V2 a where
    {-# INLINE (.*) #-}
    type Double .* V2 a = V2 'VAny
    (.*) = flip (.*)

-- |The norm (length) of a vector.
norm :: V2 a -> Double
norm = sqrt . normSq

-- |The squared norm (length) of a vector.
normSq :: V2 a -> Double
normSq v = v ⋅ v

-- |Check if a vector is non-zero
nonZero :: V2 a -> Maybe (V2 'VNonZero)
nonZero v
    | v .== V2 0 0  = Nothing
    | otherwise    = Just (unsafeCoerce v)

-- |The normalized vector.
normalize :: (NonZero a) => V2 a -> V2 'VUnit
normalize v = unsafeCoerce (v ./ norm v)

v2Cos :: Unit a => V2 a -> Double
v2Cos (V2 c _) = c

v2Sin :: Unit a => V2 a -> Double
v2Sin (V2 _ s) = s

v2ToRad :: Unit a => V2 a -> Rad
v2ToRad = Rad . acos . v2Cos

radToV2 :: Rad -> V2 'VUnit
radToV2 (Rad r) = unsafeCoerce $ V2 (cos r) (sin r)

-- |Downgrade the properties
downgrade :: (a ~> b) => V2 a -> V2 b
downgrade = unsafeCoerce

instance P2 :+ V2 a where
    type P2 .+ V2 a = P2
    (P2 px py) .+ (V2 vx vy) = P2 (px + vx) (py + vy)

instance P2 :- P2 where
    type P2 .- P2 = V2 'VAny
    (P2 x1 y1) .- (P2 x2 y2) = V2 (x1 - x2) (y1 - y2)

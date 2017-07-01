{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Prim.Classes (
      Prim (..)
    , ClosestPoints (..)
    , Distance (..)
    , (:⊆) (..)
    , (:∩?) (..)
    , (:∩) (..)
    , (:∪) (..)
) where

import           Alpaca.Geo.Prim.P2

-- |All primitive geometries represent a set of points
class Prim a where
    (∈) :: P2 -> a -> Bool

class ClosestPoints a b where
    -- |closestPoints on a and b
    -- ArgMin (u ∈ a, v ∈ b). |u-v|
    closestPoints :: a -> b -> (P2, P2)

class Distance a b where
    -- |Minmum distance between a and b.
    -- Min (u ∈ a, v ∈ b). |u-v|
    distance :: a -> b -> Double
    distance a b = let dSq = distanceSq a b in sqrt dSq

    -- |Minmum distance between a and b.
    -- Min (u ∈ a, v ∈ b). |u-v|²
    distanceSq :: a -> b -> Double
    distanceSq a b = let d = distance a b in d * d

-- |Contains test (a ⊆ b iff b contains a)
class a :⊆ b where
    (⊆) :: a -> b -> Bool

-- |Intersection test (a ⊆ b iff b contains a)
class a :∩? b where
    (∩?) :: a -> b -> Bool

-- |Intersection
class a :∩ b where
    type a ∩ b
    (∩) :: a -> b -> a ∩ b

-- |Union
class a :∪ b where
    type a ∪ b
    (∪) :: a -> b -> a ∪ b

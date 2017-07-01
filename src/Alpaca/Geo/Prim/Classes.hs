{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Prim.Classes (
      Prim (..)
    , (:⊆) (..)
    , (:∩?) (..)
    , (:∩) (..)
    , (:∪) (..)
    , Distance (..)
    , ClosestPoints (..)
) where

import           Alpaca.Geo.Prim.P2

-- |All primitive geometries represent a set of points
class Prim a where
    (∈) :: P2 -> a -> Bool

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

class Distance a b where
    -- |Minmum distance between a and b.
    -- Min (u ∈ a, v ∈ b). |u-v|
    distance :: a -> b -> Double

    -- |Minmum distance between a and b.
    -- Min (u ∈ a, v ∈ b). |u-v|²
    distanceSq :: a -> b -> Double

class ClosestPoints a b where
    -- |closestPoints on a and b
    -- ArgMin (u ∈ a, v ∈ b). |u-v|
    closestPoints :: a -> b -> (P2, P2)

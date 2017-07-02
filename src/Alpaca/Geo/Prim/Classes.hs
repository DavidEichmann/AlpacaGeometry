{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Prim.Classes (
      Prim (..)
    , ClosestPoints (..)
    , Distance (..)
    , distanceSqViaClosestPoints
    , Area (..)
    , Center (..)
    , (:⊆) (..)
    , (:∩?) (..)
    , (:∩) (..)
    , (:∪) (..)
) where

import           Alpaca.Geo.HMath
import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2

-- |All primitive geometries represent a set of points
class Prim a where
    (∈) :: P2 -> a -> Bool

instance Prim P2 where (∈) = (==)

class ClosestPoints a b where
    -- |closestPoints on a and b
    -- ArgMin (u ∈ a, v ∈ b). |u-v|
    closestPoints :: a -> b -> (P2, P2)

instance ClosestPoints P2 P2 where closestPoints a b = (a, b)

class Distance a b where
    -- |Minmum distance between a and b.
    -- Min (u ∈ a, v ∈ b). |u-v|
    distance :: a -> b -> Double
    distance a b = let dSq = distanceSq a b in sqrt dSq

    -- |Minmum distance between a and b.
    -- Min (u ∈ a, v ∈ b). |u-v|²
    distanceSq :: a -> b -> Double
    distanceSq a b = let d = distance a b in d * d

instance Distance P2 P2 where
    distance   a b = norm   (a .- b)
    distanceSq a b = normSq (a .- b)

distanceSqViaClosestPoints :: ClosestPoints a b => a -> b -> Double
distanceSqViaClosestPoints a b = let (ap, bp) = closestPoints a b in distanceSq ap bp

class Area a where
    area :: a -> Double

instance Area P2 where
    area _ = 0

class Center a where
    center :: a -> P2

instance Center P2 where
    center = id

-- |Contains test (a ⊆ b iff b contains a)
class a :⊆ b where
    (⊆) :: a -> b -> Bool

instance Prim p => P2 :⊆ p where
    (⊆) = (∈)

-- |Intersection test (a ⊆ b iff b contains a)
class a :∩? b where
    (∩?) :: a -> b -> Bool

instance Prim p => P2 :∩? p where
    (∩?) = (∈)

instance Prim p => p :∩? P2 where
    (∩?) = flip (∈)

-- |Intersection
class a :∩ b where
    type a ∩ b
    (∩) :: a -> b -> a ∩ b

-- |Union
class a :∪ b where
    type a ∪ b
    (∪) :: a -> b -> a ∪ b

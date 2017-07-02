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
    , ConvexPolygonShape (..)
    , Caster (..)
    , Cast (..)
) where

import           Data.Function      (on)
import           Data.Maybe         (mapMaybe)
import           Safe               (minimumByMay)

import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2
import           Alpaca.HMath

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

-- |Cast is like intersection, but is only conserned with the "first" intersection point.
-- This can be thought of as the minimum distance point of the intersection given some
-- distance function. For example, a **ray cast**, is the closet point from the ray start,
-- to the intersection of the ray and another set of points.
-- Note that his implies a distance function for the primitive. Not all primitives have
-- an obvious function, or may have many that are equaly relevant. When implementing
-- Cast, make sure to avoid abstraction leaks, and only implement it if there is a single
-- obvious implementation (if there are many, consider using newtypes).
class Prim c => Caster c where
    -- Cast-distance squared to a point. This is assumes point intersects self.
    casterDistanceSq :: c -> P2 -> Double;
    casterDistanceSq c p = let dist = casterDistance c p in dist * dist

    -- Cast distance to a point. This is assumes point intersects self.
    casterDistance :: c -> P2 -> Double
    casterDistance c p = sqrt (casterDistanceSq c p)

class Caster c => Cast c a where
    -- Cast self onto the rhs. If there is an intersection, then this returns
    -- the "first" point on the intersection with respect to the Caster::cast_dist_p2_sqr_unchecked distance
    -- function defined for Self.
    cast :: c -> a -> Maybe P2

    castDistanceSq :: c -> a -> Maybe Double
    castDistanceSq c a = casterDistanceSq c <$> cast c a

    castDistance :: c -> a -> Maybe Double
    castDistance c a = casterDistance c <$> cast c a

    castDistanceSqP2 :: c -> a -> Maybe (Double, P2)
    castDistanceSqP2 c a = do
        p <- cast c a
        return (casterDistanceSq c p, p)

    castDistanceP2 :: c -> a -> Maybe (Double, P2)
    castDistanceP2 c a =  do
        p <- cast c a
        return (casterDistance c p, p)


-- Cast over a list of casts.
instance (Cast c a) => Cast c [a] where
    cast c as = snd <$> minimumByMay (compare `on` fst) (mapMaybe (castDistanceSqP2 c) as)

-- |Convex Polygon Shapes
class ConvexPolygonShape a where
    corners :: a -> [P2]
    -- TODO edges and halfspaces

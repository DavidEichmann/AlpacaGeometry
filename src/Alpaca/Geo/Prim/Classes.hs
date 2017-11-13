{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DefaultSignatures     #-}

module Alpaca.Geo.Prim.Classes (
      Prim (..)
    , ClosestPoints (..)
    , Distance (..)
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

-- |All primitive geometries denote a non-empty set of points defined by (∈)
-- i.e. (∈) is the characteristic function of the set of points.
class Prim a where
    -- |∈ denotes set containment.
    (∈) :: P2 -> a -> Bool

-- |A P2 is itself a primitive: ⟦P2 x y⟧ = {(x,y)}
-- Note that there is now a double denotation. P2
-- is both a point (⟦P2 x y⟧ = (x,y)) and a singleton set
-- (⟦P2 x y⟧ = {(x,y)}). Which definition is in use is usually clear from the
-- context and there is little to gain from being explicite about the difference.
-- Hence:
--    ⟦P2 x y ∈ P2 u v⟧
--    = ⟦P2 x y⟧ ⟦∈⟧ ⟦P2 u v⟧
--    = (x, y) ∈ {(u v)}              -- Note dule denotation here
--    = (x, y) == (u v)
instance Prim P2 where (∈) = (==)

class (Prim a, Prim b) => ClosestPoints a b where
    -- |closestPoints on a and b
    -- ⟦closestPoints a b⟧ = ArgMin_{(u, v) ∈ a×b} ‖u-v‖
    closestPoints :: a -> b -> (P2, P2)

-- | ⟦closestPoints a b⟧
--   = ArgMin_{(u, v) ∈ {a}×{b}} ‖u-v‖
--   = ArgMin_{(u, v) ∈ {(a, b)}} ‖u-v‖
--   = (a, b)
instance ClosestPoints P2 P2 where closestPoints a b = (a, b)

class (Prim a, Prim b) => Distance a b where
    -- |Minmum distance between a and b.
    --   Min_{(u, v) ∈ a×b} ‖u-v‖
    distance :: a -> b -> Double
    distance a b = sqrt (distanceSq a b)

    -- |Minmum distance between a and b.
    --   Min_{(u, v) ∈ a×b}. ‖u-v‖²
    -- Note the parallel with closest point leads to this default implementation.
    distanceSq :: a -> b -> Double
    default distanceSq :: ClosestPoints a b => a -> b -> Double
    distanceSq a b = let (u, v) = closestPoints a b in normSq (u .- v)

instance Distance P2 P2 where
    -- | ⟦distance a b⟧
    --   = Min_{(u, v) ∈ {a}×{b}} ‖u-v‖
    --   = Min_{(u, v) ∈ {(a, b)}} ‖u-v‖
    --   = ‖a-b‖
    distance   a b = norm   (a .- b)

    -- | ⟦distance a b⟧
    --   = Min_{(u, v) ∈ {a}×{b}} ‖u-v‖²
    --   = Min_{(u, v) ∈ {(a, b)}} ‖u-v‖²
    --   = ‖a-b‖²
    distanceSq a b = normSq (a .- b)

class Prim a => Area a where
    -- |The geometrical area of the primitive.
    area :: a -> Double

instance Area P2 where
    area _ = 0

class Prim a => Center a where
    -- |The geometric center. Mean of all the points in the primitive.
    -- Note that if the area is 0, then this reduces to the perimiter center.
    center :: a -> P2

instance Center P2 where
    -- | ⟦center a⟧
    --   = Mean {a}
    --   = a
    center = id


-- TODO continue documenting and improving type classes.
-- TODO introduce approximate versions of these type classes


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

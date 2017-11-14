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
) where

import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2
import           Alpaca.HMath

-- |All primitive geometries denote a non-empty set of points defined by (∈)
-- i.e. (∈) is the characteristic function of the set of points.
class Prim a where
  -- |∈ denotes set containment.
  --   ⟦a ∈ b⟧ = a ∈ b
  (∈) :: P2 -> a -> Bool

-- |A P2 is itself a primitive: ⟦P2 x y⟧ = {(x,y)}
-- Note that there is now a double denotation. P2
-- is both a point (⟦P2 x y⟧ = (x,y)) and a singleton set
-- (⟦P2 x y⟧ = {(x,y)}). Which definition is in use is usually clear from the
-- context and there is little to gain from being explicite about the difference.
instance Prim P2 where
  -- | ⟦P2 x y ∈ P2 u v⟧
  --   = ⟦P2 x y⟧ ⟦∈⟧ ⟦P2 u v⟧
  --   = (x, y) ∈ {(u v)}              -- Note dule denotation here
  --   = (x, y) == (u v)
  (∈) = (==)

class (Prim a, Prim b) => ClosestPoints a b where
  -- |closestPoints on a and b
  -- ⟦closestPoints a b⟧ = ArgMin_{(u, v) ∈ a×b} ‖u-v‖
  closestPoints :: a -> b -> (P2, P2)

instance ClosestPoints P2 P2 where
  -- | ⟦closestPoints a b⟧
  --   = ArgMin_{(u, v) ∈ {a}×{b}} ‖u-v‖
  --   = ArgMin_{(u, v) ∈ {(a, b)}} ‖u-v‖
  --   = (a, b)
  --   = ⟦(a, b)⟧
  closestPoints a b = (a, b)

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
  distance a b = norm (a .- b)

  -- | ⟦distance a b⟧
  --   = Min_{(u, v) ∈ {a}×{b}} ‖u-v‖²
  --   = Min_{(u, v) ∈ {(a, b)}} ‖u-v‖²
  --   = ‖a-b‖²
  distanceSq a b = normSq (a .- b)

class a :⊆ b where
  -- |Contains test. This is simply set subset-or-equal.
  --
  (⊆) :: a -> b -> Bool

instance Prim a => P2 :⊆ a where
  -- | ⟦p ⊆ a⟧
  -- = {p} ⊆ a
  -- = p ∈ a
  -- = ⟦p ∈ a⟧
  (⊆) = (∈)

class a :∩? b where
  -- |Contains test. This is simply set intersection.
  --   ⟦a ∩? b⟧
  -- = a ∩ b /= ∅
  (∩?) :: a -> b -> Bool

instance Prim a => P2 :∩? a where
  -- | ⟦p ∩? a⟧
  -- = {p} ∩ a /= ∅
  -- = p ∈ a
  -- = ⟦p ∈ a⟧
  (∩?) = (∈)

instance Prim a => a :∩? P2 where
  (∩?) = flip (∈)

class a :∩ b where
  -- |The type of the intersection.
  type a ∩ b

  -- |This is simply set intersection.
  --   ⟦a ∩ b⟧
  -- = a ∩ b
  (∩) :: a -> b -> a ∩ b

-- |Union
class a :∪ b where
  -- |The type of the union.
  type a ∪ b

  -- |This is simply set union.
  --   ⟦a ∪ b⟧
  -- = a ∪ b
  (∪) :: a -> b -> a ∪ b

-- |Convex Polygon Shapes
class ConvexPolygonShape a where
  corners :: a -> [P2]
  -- TODO edges and halfspaces


class Prim a => Center a where
  -- |The geometric center. Mean of all the points in the primitive.
  -- Note that if the area is 0, then this reduces to the perimiter center.
  center :: a -> P2

instance Center P2 where
  -- | ⟦center a⟧
  --   = Mean {a}
  --   = a
  center = id


class Prim a => Area a where
  -- |The geometrical area of the primitive.
  area :: a -> Double

instance Area P2 where
  area _ = 0

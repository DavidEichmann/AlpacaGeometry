{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Prim (
      LineIntersectLine (..)
    , aaboxFromCenterWidthHeight
    , aaboxAspect
    , p2ToV2
    , v2ToP2
    , module Export
) where

import           Data.List               (foldl1')

import           Alpaca.Geo.HMath
import           Alpaca.Geo.Prim.AABox   as Export
import           Alpaca.Geo.Prim.Classes as Export
import           Alpaca.Geo.Prim.Line    as Export
import           Alpaca.Geo.Prim.P2      as Export
import           Alpaca.Geo.Prim.V2      as Export

instance P2 :+ V2 a where
    type P2 .+ V2 a = P2
    (P2 px py) .+ (V2 vx vy) = P2 (px + vx) (py + vy)

instance P2 :+ P2 where
    type P2 .+ P2 = P2
    (P2 x1 y1) .+ (P2 x2 y2) = P2 (x1 + x2) (y1 + y2)

instance P2 :- V2 a where
    type P2 .- V2 a = P2
    (P2 px py) .- (V2 vx vy) = P2 (px - vx) (py - vy)

instance P2 :- P2 where
    type P2 .- P2 = V2 'VAny
    (P2 x1 y1) .- (P2 x2 y2) = V2 (x1 - x2) (y1 - y2)

instance P2 :* Double where
    type P2 .* Double = P2
    (P2 x1 y1) .* s = P2 (x1 * s) (y1 * s)

instance Double :* P2 where
    type Double .* P2 = P2
    (.*) = flip (.*)

-- Prim

instance Prim P2 where
    (∈) = (==)

instance Prim Line where
    p ∈ Line lp ld = (lp .- p) × ld == 0

-- Equality

instance Eq Line where
    (Line p1 d1) == (Line p2 d2) = (d1 × d2 == 0) && ((p1 .- p2) × d1 == 0)

-- (Maybe)AABounded

instance AABounded a => MaybeAABounded [a] where
    maybeAabb [] = Nothing
    maybeAabb as = Just (foldl1' aaboxUnion (fmap aabb as))

-- ClosestPoints

instance ClosestPoints P2 P2 where closestPoints = (,)

instance ClosestPoints P2 Line where
    closestPoints = flip closestPoints
instance ClosestPoints Line P2 where
    closestPoints l x = (lineAt l (closestPointTime l x), x)

instance ClosestPoints Line Line where
    closestPoints a b = case a ∩ b of
        LILNothing -> closestPoints (lineP a) b
        LILPoint p -> (p, p)
        LILLine _  -> let p = lineP a in (p, p)

-- Distance

distanceSqViaClosestPoints :: ClosestPoints a b => a -> b -> Double
distanceSqViaClosestPoints a b = let (ap, bp) = closestPoints a b in distanceSq ap bp

instance Distance P2 P2 where
    distance   a b = norm   (a .- b)
    distanceSq a b = normSq (a .- b)

instance Distance P2 Line where
    distance   = flip distance
    distanceSq = flip distanceSq
instance Distance Line P2 where
    distanceSq = distanceSqViaClosestPoints

-- Area

instance Area P2 where
    area _ = 0
instance Area Line where
    area _ = 0
instance Area AABox where
    area box = let V2 w h = aaboxDiagonal box in w * h

-- Center

instance Center P2 where
    center = id
instance Center AABox where
    center box = (aaboxMin box + aaboxMax box) .* (0.5 :: Double)

-- Intersection (test)

instance Line :∩ Line where
    type Line ∩ Line = LineIntersectLine
    l1@(Line p1 d1) ∩ l2 = case intersectionTime l1 l2 of
        LILTNothing  -> LILNothing
        LILTPoint t1 -> LILPoint (p1 .+ (t1 .* d1))
        LILTLine     -> LILLine l1

instance Line :∩? Line where
    l1 ∩? l2 = intersectionTime l1 l2 == LILTNothing

data LineIntersectLine
    = LILNothing
    | LILPoint P2
    | LILLine Line
    deriving (Eq)

data LineIntersectLineTime
    = LILTNothing
    | LILTPoint Double
    | LILTLine
    deriving (Eq)

-- |The time parameter of the first line that intersects the second line.
intersectionTime :: Line -> Line -> LineIntersectLineTime
intersectionTime (Line p1 d1) (Line p2 d2)
    | d2xd1 /= 0  = LILTPoint (p21xd2 / d2xd1)
    | p21xd2 == 0 = LILTLine
    | otherwise   = LILTNothing
    where
        d2xd1 = d2 × d1
        p21xd2 = (p1 .- p2) × d2

-- |The time parameter of the closest point on a line to anther point.
closestPointTime :: Line -> P2 -> Double
closestPointTime (Line p d) x = (d ⋅ (x .- p)) / normSq d

-- TODO SegToLine
-- TODO rayToLine


aaboxFromCenterWidthHeight :: P2 -> Double -> Double -> AABox
aaboxFromCenterWidthHeight c w h = let
    halfDiagonal = V2 (w * 0.5) (h * 0.5)
    in aabb (c .- halfDiagonal, c .+ halfDiagonal)

aaboxAspect :: AABox -> Maybe Double
aaboxAspect box = case aaboxDiagonal box of
    (V2 _ 0) -> Nothing
    (V2 w h) -> Just (w / h)

aaboxGrowToAspect :: Maybe Double -> AABox -> AABox
aaboxGrowToAspect Nothing box = undefined

aaboxDiagonal :: AABox -> V2 'VAny
aaboxDiagonal box = aaboxMax box .- aaboxMin box

lineAt :: Line -> Double -> P2
lineAt (Line p d) t = p .+ (t .* d)

-- P2 convertions

p2ToV2 :: P2 -> V2 'VAny
p2ToV2 (P2 x y) = V2 x y

-- V2 convertions

v2ToP2 :: V2 a -> P2
v2ToP2 (V2 x y) = P2 x y

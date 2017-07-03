{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Alpaca.Geo.Prim.Circle (
      Circle
    , circle
    , radius
) where

import           Alpaca.Geo.Prim.AABox
import           Alpaca.Geo.Prim.Classes
import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.Ray
import           Alpaca.Geo.Prim.V2
import           Alpaca.HMath

-- |An axis aligned box.
data Circle = Circle P2 Double
    deriving (Eq)

circle :: P2 -> Double -> Maybe Circle
circle c r
    | r < 0     = Nothing
    | otherwise = Just (Circle c r)

radius :: Circle -> Double
radius (Circle _ r) = r

instance Prim Circle where
    p ∈ (Circle c r) = distanceSq p c <= r * r

instance Area Circle where
    area (Circle _ r) = pi * r * r

instance Center Circle where
    center (Circle c _) = c

instance AABounded Circle where
    aabb (Circle c r) = let dim = r + 2 in fromCenterWidthHeight c dim dim

instance ClosestPoints P2 Circle where
    closestPoints = flip closestPoints

instance ClosestPoints Circle P2 where
    closestPoints (Circle c r) p
        = let
            cToP = p .- c
            cpDistSq = normSq cToP
        in if cpDistSq > r * r
            -- Note that cpDistSq must be greater than 0.0 at this point so it is safe to divide here.
            then (c .+ ((cToP ./ sqrt cpDistSq) .* r), p)
            else (p, p)

instance Distance P2 Circle where
    distance = flip distance

instance Distance Circle P2 where
    distance (Circle c r) p = max 0 (distance c p - r)

instance Circle :⊆ Circle where
    Circle c1 r1 ⊆ Circle c2 r2 = r1 + distance c1 c2 <= r2

-- TODO Cast
-- TODO transformable

instance Cast Ray Circle where
    cast ray@(Ray rayP rayD) (Circle circleC circleR) = let
        p_to_c = circleC .- rayP;

        -- a, b, and c of the quadratic formula.
        a = normSq rayD;
        b = -2.0 * (p_to_c ⋅ rayD)
        c = normSq p_to_c - (circleR * circleR);
        u = (b * b) - (4.0 * a * c)

        in if
            | u < 0.0   -> Nothing
            | u == 0.0  -> let t = -b / (2.0 * a)
                in if t >= 0.0
                    then Just (rayAt ray t)
                    else Nothing
            | otherwise -> let
                v = sqrt u
                denom = 2.0 * a

                -- Note that a is always positive so lo and high are clear.
                lo_t = ((-b) - v) / denom;
                in if lo_t >= 0.0
                    then Just (rayAt ray lo_t)
                    else let hi_t = ((-b) + v) / denom;
                        in if hi_t >= 0.0
                            -- Ray starts inside the circle.
                            -- This is a filled circle hence use ray start.
                            then Just rayP
                            else Nothing

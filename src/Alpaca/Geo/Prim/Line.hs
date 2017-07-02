{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Alpaca.Geo.Prim.Line (
      Line (..)
) where

import           Alpaca.HMath
import           Alpaca.Geo.Prim.Classes
import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2

-- |A line.
data Line = Line { lineP :: P2, lineDir :: V2 'VNonZero }

instance Prim Line where
    p ∈ Line lp ld = (lp .- p) × ld == 0

instance Eq Line where
    (Line p1 d1) == (Line p2 d2) = (d1 × d2 == 0) && ((p1 .- p2) × d1 == 0)

instance ClosestPoints P2 Line where
    closestPoints = flip closestPoints

instance ClosestPoints Line P2 where
    closestPoints l x = (lineAt l (closestPointTime l x), x)

instance ClosestPoints Line Line where
    closestPoints a b = case a ∩ b of
        LILNothing -> closestPoints (lineP a) b
        LILPoint p -> (p, p)
        LILLine _  -> let p = lineP a in (p, p)

instance Distance P2 Line where
    distance   = flip distance
    distanceSq = flip distanceSq
instance Distance Line P2 where
    distanceSq = distanceSqViaClosestPoints

instance Area Line where
    area _ = 0

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

lineAt :: Line -> Double -> P2
lineAt (Line p d) t = p .+ (t .* d)

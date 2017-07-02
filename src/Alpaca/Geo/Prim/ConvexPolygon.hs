{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Prim.ConvexPolygon (
      ConvexPolygon
    , corners
    , convexHull
) where

import           Data.List               (sort)
import           Data.Maybe              (fromJust)

import           Alpaca.Geo.Prim.AABox   (AABounded (..), MaybeAABounded (..))
import           Alpaca.Geo.Prim.Classes
import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2
import           Alpaca.HMath

-- |CCW ordered list of convex points
newtype ConvexPolygon = ConvexPolygon [P2]

-- TODO instance prim
-- TODO area
-- TODO center
-- TODO ray cast
-- TODO ray cast

instance ConvexPolygonShape ConvexPolygon where
    corners (ConvexPolygon ps) = ps

instance AABounded ConvexPolygon where
    aabb (ConvexPolygon ps) = fromJust (maybeAabb ps)

convexHull :: [P2] -> Maybe ConvexPolygon
convexHull []  = Nothing
convexHull [a] = Just (ConvexPolygon [a])
convexHull ps  = let

    -- |Return the half hull in reverse order.
    -- Input must be sorted along the x axis (increasing x gives the lower
    -- hull, decreasing x gives the upper hull).
    revHalfHull :: [P2] -> [P2]
    revHalfHull (n:ns) = revHalfHull' n [] ns
    revHalfHull []     = error "Impossible"

    -- |Takes current point and current hull in reverse order and the the remaining points.
    revHalfHull' :: P2 -> [P2] -> [P2] -> [P2]
    revHalfHull' h revHull [] = h:revHull
    revHalfHull' h1 [] (n:ns) = revHalfHull' n [h1] ns
    revHalfHull' h1 (h2:hs) (n:ns)
        | isCCW h2 h1 n = revHalfHull' n (h1:h2:hs) ns
        | otherwise     = revHalfHull' h2 hs (n:ns)

    isCCW :: P2 -> P2 -> P2 -> Bool
    isCCW a b c = (b .- a) × (c .- b) > 0

    orderedPs = sort ps
    lowerHull = reverse (revHalfHull orderedPs)
    revUpperHull = revHalfHull (reverse orderedPs)

    in Just (ConvexPolygon (tail lowerHull ++ tail revUpperHull))

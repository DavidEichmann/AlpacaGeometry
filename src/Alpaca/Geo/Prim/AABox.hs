{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Alpaca.Geo.Prim.AABox (
      AABounded (..)
    , MaybeAABounded (..)
    , AABox
    , AspectErr (..)
    , pMin
    , pMax
    , fromCenterWidthHeight
    , aspect
    , growToAspect
    , diagonal
    , union
) where

import           Data.List               (foldl1')

import           Alpaca.Geo.Prim.Classes
import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2
import           Alpaca.HMath

-- |An axis aligned box.
data AABox = AABox { pMin :: P2, pMax :: P2 }
    deriving (Eq)

instance Prim AABox where
    (P2 x y) ∈ (AABox (P2 loX loY) (P2 hiX hiY))
        = loX <= x && x <= hiX &&
          loY <= y && y <= hiY

instance ConvexPolygonShape AABox where
    corners (AABox lo@(P2 loX loY) hi@(P2 hiX hiY)) = [
            lo,
            P2 hiX loY,
            hi,
            P2 loX hiY
        ]

instance Area AABox where
    area box = let V2 w h = diagonal box in w * h

instance Center AABox where
    center box = (pMin box + pMax box) .* (0.5 :: Double)

instance ClosestPoints P2 AABox where
    closestPoints = flip closestPoints

instance ClosestPoints AABox P2 where
    closestPoints (AABox (P2 loX loY) (P2 hiX hiY)) p@(P2 x y)
        = (P2 (min (max loX x) hiX)
              (min (max loY y) hiY), p)

instance Distance P2 AABox where distanceSq = distanceSqViaClosestPoints
instance Distance AABox P2 where distanceSq = distanceSqViaClosestPoints




-- TODO instance contains Seg
-- TODO instance cast Ray


fromCenterWidthHeight :: P2 -> Double -> Double -> AABox
fromCenterWidthHeight c w h = let
    halfDiagonal = V2 (w * 0.5) (h * 0.5)
    in aabb (c .- halfDiagonal, c .+ halfDiagonal)

data AspectErr = ZeroWidth | ZeroHeight

aspect :: AABox -> Either AspectErr Double
aspect box = case diagonal box of
    (V2 0 0) -> Right 1
    (V2 0 _) -> Left ZeroWidth
    (V2 _ 0) -> Left ZeroHeight
    (V2 w h) -> Right (w / h)

withMargins :: AABox -> Double -> Double -> Double -> Double -> AABox
withMargins
    box@(AABox (P2 loX loY) (P2 hiX hiY))
    top
    right
    bottom
    left
    = let P2 cx cy = center box
      in AABox (P2 (min cx (loX - left))  (min cy (loY - bottom)))
               (P2 (max cx (hiX + right)) (max cy (hiY + top)))

growToAspect :: Double -> AABox -> Either AspectErr AABox
growToAspect newAspect box = do
    boxAspect <- aspect box
    let V2 boxDiagonalX boxDiagonalY = diagonal box
    if boxAspect < newAspect
    then
        -- Grow along x
        let halfDx = ((newAspect * boxDiagonalY)- boxDiagonalX) * 0.5
        in return (withMargins box 0.0 halfDx 0.0 halfDx)
    else
        -- Grow along y
        let half_dy = ((boxDiagonalY / newAspect) - boxDiagonalY) * 0.5
        in return (withMargins box half_dy 0.0 half_dy 0.0)

diagonal :: AABox -> V2 'VAny
diagonal box = pMax box .- pMin box


class AABounded a where
    aabb :: a -> AABox

class MaybeAABounded a where
    maybeAabb :: a -> Maybe AABox

instance AABounded a => MaybeAABounded [a] where
    maybeAabb [] = Nothing
    maybeAabb as = Just (foldl1' union (fmap aabb as))

instance AABounded AABox where
    aabb = id

instance AABounded P2 where
    aabb p = AABox p p

instance AABounded (P2, P2) where
    aabb (P2 x1 y1, P2 x2 y2) = AABox
        (P2 (min x1 x2) (min y1 y2))
        (P2 (max x1 x2) (max y1 y2))

-- TODO aaboxEdges :: AABox -> [Seg]

union :: AABox -> AABox -> AABox
union
    (AABox (P2 minX1 minY1) (P2 maxX1 maxY1))
    (AABox (P2 minX2 minY2) (P2 maxX2 maxY2))
    = AABox
        (P2 (min minX1 minX2) (min minY1 minY2))
        (P2 (max maxX1 maxX2) (max maxY1 maxY2))

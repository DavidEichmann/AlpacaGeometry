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
    , aaboxMin
    , aaboxMax
    , fromCenterWidthHeight
    , aspect
    , growToAspect
    , diagonal
    , corners
    , union
) where

import           Data.List               (foldl1')

import           Alpaca.Geo.HMath
import           Alpaca.Geo.Prim.Classes
import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2

-- |An axis aligned box.
data AABox = AABox { aaboxMin :: P2, aaboxMax :: P2 }
    deriving (Eq)

instance Area AABox where
    area box = let V2 w h = diagonal box in w * h

instance Center AABox where
    center box = (aaboxMin box + aaboxMax box) .* (0.5 :: Double)

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

withMarginsUnsafe :: AABox -> Double -> Double -> Double -> Double -> AABox
withMarginsUnsafe
    (AABox (P2 loX loY) (P2 hiX hiY))
    top
    right
    bottom
    left
    = AABox (P2 (loX - left) (loY - bottom))
            (P2 (hiX + right) (hiY + top))

growToAspect :: Double -> AABox -> Either AspectErr AABox
growToAspect newAspect box = do
    boxAspect <- aspect box
    let V2 boxDiagonalX boxDiagonalY = diagonal box
    if boxAspect < newAspect
    then
        -- Grow along x
        let halfDx = ((newAspect * boxDiagonalY)- boxDiagonalX) * 0.5
        in return (withMarginsUnsafe box 0.0 halfDx 0.0 halfDx)
    else
        -- Grow along y
        let half_dy = ((boxDiagonalY / newAspect) - boxDiagonalY) * 0.5
        in return (withMarginsUnsafe box half_dy 0.0 half_dy 0.0)

diagonal :: AABox -> V2 'VAny
diagonal box = aaboxMax box .- aaboxMin box


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

corners :: AABox -> [P2]
corners (AABox lo@(P2 loX loY) hi@(P2 hiX hiY)) = [
        lo,
        P2 hiX loY,
        hi,
        P2 loX hiY
    ]

-- TODO aaboxEdges :: AABox -> [Seg]

union :: AABox -> AABox -> AABox
union
    (AABox (P2 minX1 minY1) (P2 maxX1 maxY1))
    (AABox (P2 minX2 minY2) (P2 maxX2 maxY2))
    = AABox
        (P2 (min minX1 minX2) (min minY1 minY2))
        (P2 (max maxX1 maxX2) (max maxY1 maxY2))

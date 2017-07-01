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
    , aaboxMin
    , aaboxMax
    , aaboxCorners
    , aaboxUnion
) where

import           Alpaca.Geo.Prim.P2

class AABounded a where
    aabb :: a -> AABox

class MaybeAABounded a where
    maybeAabb :: a -> Maybe AABox

-- |An axis aligned box.
data AABox = AABox { aaboxMin :: P2, aaboxMax :: P2 }
    deriving (Eq)

instance AABounded AABox where
    aabb = id

instance AABounded P2 where
    aabb p = AABox p p

instance AABounded (P2, P2) where
    aabb (P2 x1 y1, P2 x2 y2) = AABox
        (P2 (min x1 x2) (min y1 y2))
        (P2 (max x1 x2) (max y1 y2))

aaboxCorners :: AABox -> [P2]
aaboxCorners (AABox lo@(P2 loX loY) hi@(P2 hiX hiY)) = [
        lo,
        P2 hiX loY,
        hi,
        P2 loX hiY
    ]

-- TODO aaboxEdges :: AABox -> [Seg]

aaboxUnion :: AABox -> AABox -> AABox
aaboxUnion
    (AABox (P2 minX1 minY1) (P2 maxX1 maxY1))
    (AABox (P2 minX2 minY2) (P2 maxX2 maxY2))
    = AABox
        (P2 (min minX1 minX2) (min minY1 minY2))
        (P2 (max maxX1 maxX2) (max maxY1 maxY2))

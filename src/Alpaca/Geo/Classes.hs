{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Classes (
      module Export
    , Transform
    , Prim (..)
    , (:⊆) (..)
    , (:∩?) (..)
    , (:∩) (..)
    , (:∪) (..)
) where

import           Alpaca.Geo.HMath as Export
import           Alpaca.Geo.P2
import           Alpaca.Geo.V2

class (a :* P2, a :* V2) => Transform a

-- |All primitive geometries represent a set of points
class Prim a where
    (∈) :: P2 -> a -> Bool

-- |Contains test (a ⊆ b iff b contains a)
class a :⊆ b where
    (⊆) :: a -> b -> Bool

-- |Intersection test (a ⊆ b iff b contains a)
class a :∩? b where
    (∩?) :: a -> b -> Bool

-- |Intersection
class a :∩ b where
    type a ∩ b
    (∩) :: a -> b -> a ∩ b

-- |Union
class a :∪ b where
    type a ∪ b
    (∪) :: a -> b -> a ∪ b

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

module Alpaca.Geo.Prim.Ray (
      Ray (..)
    , rayAt
) where

import           Alpaca.Geo.Prim.Classes
import           Alpaca.Geo.Prim.Line
import           Alpaca.Geo.Prim.P2
import           Alpaca.Geo.Prim.V2
import           Alpaca.HMath

-- |A ray.
--   ⟦Ray p d⟧        -- (d /= (0,0))
-- = { p + td | t ∈ R . t ≥ 0 }
data Ray = Ray P2 (V2 'VNonZero)
    deriving (Eq)

instance Prim Ray where
    p ∈ (Ray s d) = let toP = p .- s in toP × d == 0 && toP ⋅ d > 0

instance Area Ray where
    area _ = 0

-- TODO transformable

data RayIntersectLine
    = RILNothing
    | RILPoint P2
    | RILRay Ray

instance Ray :∩ Line where
    type Ray ∩ Line = RayIntersectLine
    (∩) = flip (∩)
instance Line :∩ Ray where
    type Line ∩ Ray = RayIntersectLine
    line ∩ self = case lineIntersectLineTime (toLine self) line of
        LILTNothing -> RILNothing
        LILTLine    -> RILRay self
        LILTPoint t -> if t >= 0.0
                        then RILPoint (rayAt self t)
                        else RILNothing

rayAt :: Ray -> Double -> P2
rayAt (Ray s d) t = s .+ (t .* d)

toLine :: Ray -> Line
toLine (Ray s d) = Line s d

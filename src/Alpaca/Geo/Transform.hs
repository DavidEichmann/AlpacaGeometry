{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Transform where

import           Alpaca.Geo.Classes
import           Alpaca.Geo.P2
import           Alpaca.Geo.V2

newtype Trans = Trans (V2 'VAny)

instance Trans :* Trans where
    type Trans .* Trans = Trans;
    (Trans t1) .* (Trans t2) = Trans (t1 + t2)

instance Transform Trans

instance Trans :* P2 where
    type Trans .* P2 = P2;
    (Trans t) .* p = p .+ t
instance Trans :* V2 a where
    type Trans .* V2 a = V2 a;
    _ .* v = v


data Rot = Rot { rotCos :: Double, rotSin :: Double }

-- https://en.wikipedia.org/wiki/List_of_trigonometric_identities#Angle_sum_and_difference_identities
instance Rot :* Rot where
    type Rot .* Rot = Rot;
    (Rot c1 s1) .* (Rot c2 s2) = Rot
            ((s1 * c2) + (c1 * s2))
            ((c1 * c2) - (s1 * s2))

instance Transform Rot

instance Rot :* P2 where
    type Rot .* P2 = P2;
    (Rot c s) .* (P2 x y) = P2 ((c * x) - (s * y)) ((s * x) + (c * y))

instance Rot :* V2 a where
    type Rot .* V2 a = V2 a;
    (Rot c s) .* (V2 x y) = V2 ((c * x) - (s * y)) ((s * x) + (c * y))

instance Trans :* Rot where
    type Trans .* Rot = Isometry;
    t .* r = Isometry t r

data Isometry = Isometry { trans :: Trans, rot :: Rot }

instance Transform Isometry

instance Isometry :* Isometry where
    type Isometry .* Isometry = Isometry;
    (Isometry (Trans t1) r1) .* (Isometry (Trans t2) r2)
        = Isometry (Trans $ (r1 .* t2) + t1) (r1 .* r2)

instance Isometry :* P2 where
    type Isometry .* P2 = P2;
    (Isometry t r) .* p = t .* (r .* p)
instance Isometry :* V2 a where
    type Isometry .* V2 a = V2 a;
    _ .* v = v

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Alpaca.Geo.Transform (
      Translation (..)
    , Rot
    , rotation
    , rotSin
    , rotCos
    , Isometry (..)
    , M3 (..)
    , transpose
    , determinant
    , inverse
    , identityM3
) where

import           Alpaca.Geo.Prim
import           Alpaca.Geo.Rad
import           Alpaca.HMath

newtype Translation = Translation (V2 'VAny)

instance Translation :* Translation where
    type Translation .* Translation = Translation;
    (Translation t1) .* (Translation t2) = Translation (t1 + t2)

instance Translation :* P2 where
    type Translation .* P2 = P2;
    (Translation t) .* p = p .+ t
instance Translation :* V2 a where
    type Translation .* V2 a = V2 a;
    _ .* v = v


data Rot = Rot { rotCos :: Double, rotSin :: Double }

rotation :: Rad -> Rot
rotation (Rad rad) = Rot { rotCos = cos rad, rotSin = sin rad }

-- https://en.wikipedia.org/wiki/List_of_trigonometric_identities#Angle_sum_and_difference_identities
instance Rot :* Rot where
    type Rot .* Rot = Rot;
    (Rot c1 s1) .* (Rot c2 s2) = Rot
            ((s1 * c2) + (c1 * s2))
            ((c1 * c2) - (s1 * s2))

instance Rot :* P2 where
    type Rot .* P2 = P2;
    (Rot c s) .* (P2 x y) = P2 ((c * x) - (s * y)) ((s * x) + (c * y))

instance Rot :* V2 a where
    type Rot .* V2 a = V2 a;
    (Rot c s) .* (V2 x y) = V2 ((c * x) - (s * y)) ((s * x) + (c * y))

instance Translation :* Rot where
    type Translation .* Rot = Isometry;
    t .* r = Isometry t r

data Isometry = Isometry { trans :: Translation, rot :: Rot }

instance Isometry :* Isometry where
    type Isometry .* Isometry = Isometry;
    (Isometry (Translation t1) r1) .* (Isometry (Translation t2) r2)
        = Isometry (Translation $ (r1 .* t2) + t1) (r1 .* r2)

instance Isometry :* P2 where
    type Isometry .* P2 = P2;
    (Isometry t r) .* p = t .* (r .* p)
instance Isometry :* V2 a where
    type Isometry .* V2 a = V2 a;
    _ .* v = v

data M3 = M3 Double Double Double Double Double Double Double Double Double

instance M3 :* M3 where
    type M3 .* M3 = M3;
    (M3 a00 a01 a02 a10 a11 a12 a20 a21 a22) .* (M3 b00 b01 b02 b10 b11 b12 b20 b21 b22)
        = M3 (a00 * b00 + a01 * b10 + a02 * b20)
             (a00 * b01 + a01 * b11 + a02 * b21)
             (a00 * b02 + a01 * b12 + a02 * b22)
             (a10 * b00 + a11 * b10 + a12 * b20)
             (a10 * b01 + a11 * b11 + a12 * b21)
             (a10 * b02 + a11 * b12 + a12 * b22)
             (a20 * b00 + a21 * b10 + a22 * b20)
             (a20 * b01 + a21 * b11 + a22 * b21)
             (a20 * b02 + a21 * b12 + a22 * b22)

instance M3 :* P2 where
    type M3 .* P2 = P2
    (M3 m00 m01 m02 m10 m11 m12 m20 m21 m22) .* (P2 x y)
        = P2 ((m00 * x + m01 * y + m02) * w_inv)
             ((m10 * x + m11 * y + m12) * w_inv)
        where
            w_inv = m20 * x + m21 * y + m22;

instance M3 :* Double where
    type M3 .* Double = M3
    (M3 m00 m01 m02 m10 m11 m12 m20 m21 m22) .* s
        = M3 (m00 * s) (m01 * s) (m02 * s)
             (m10 * s) (m11 * s) (m12 * s)
             (m20 * s) (m21 * s) (m22 * s)

instance M3 :/ Double where
    type M3 ./ Double = M3;
    m ./ s = m .* (1 / s)

transpose :: M3 -> M3
transpose (M3 m00 m01 m02
              m10 m11 m12
              m20 m21 m22)
    = M3 m00 m10 m20
         m01 m11 m21
         m02 m12 m22

determinant :: M3 -> Double
determinant (M3 m00 m01 m02
                m10 m11 m12
                m20 m21 m22)
    = (m00 * m11 * m22) + (m01 * m12 * m20) +
      (m02 * m10 * m21) - (m02 * m11 * m20) -
      (m01 * m10 * m22) - (m00 * m12 * m21)

inverse :: M3 -> Maybe M3
inverse m@(M3 m00 m01 m02
              m10 m11 m12
              m20 m21 m22)
    = case determinant m of
        0   -> Nothing
        det -> Just $ M3 (determinant2 m11 m12 m21 m22)
                         (determinant2 m21 m22 m01 m02)
                         (determinant2 m01 m02 m11 m12)
                         (determinant2 m20 m22 m10 m12)
                         (determinant2 m00 m02 m20 m22)
                         (determinant2 m10 m12 m00 m02)
                         (determinant2 m10 m11 m20 m21)
                         (determinant2 m20 m21 m00 m01)
                         (determinant2 m00 m01 m10 m11) ./ det
    where
        determinant2 :: Double -> Double -> Double -> Double -> Double
        determinant2 q00 q01 q10 q11 = q00 * q11 - q01 * q10

identityM3 :: M3
identityM3 = M3 1 0 0
                0 1 0
                0 0 1

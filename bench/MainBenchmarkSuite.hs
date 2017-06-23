{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Vector as V

import Data.Maybe (isJust)

import GameGeometry
import Linear
import Criterion.Main
import Control.DeepSeq
import GHC.Generics


r2 :: Rational -> Rational -> V2 Rational
r2 = V2

-- {-# SPECIALIZE (!*!) :: M33 Double -> M33 Double -> M33 Double #-}
-- {-# SPECIALIZE (^+^) :: V3 Double -> V3 Double -> V3 Double #-}
-- {-# SPECIALIZE (*^) :: Double -> V3 Double -> V3 Double #-}
-- {-# SPECIALIZE fmap :: (V3 Double -> V3 Double) -> M33 Double -> M33 Double #-}

data MMM = MMM {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
       deriving (Generic,NFData)

matrixMult :: MMM -> MMM -> MMM
matrixMult (MMM a1 b1 c1
                d1 e1 f1
                g1 h1 i1)
           (MMM a2 b2 c2
                d2 e2 f2
                g2 h2 i2) =
           (MMM (a1*a2 + b1*d2 + c1*g2) (a1*b2 + b1*e2 + c1*h2) (a1*c2 + b1*f2 + c1*i2)
                (d1*a2 + e1*d2 + f1*g2) (d1*b2 + e1*e2 + f1*h2) (d1*c2 + e1*f2 + f1*i2)
                (g1*a2 + h1*d2 + i1*g2) (g1*b2 + h1*e2 + i1*h2) (g1*c2 + h1*f2 + i1*i2))


main :: IO ()
main = defaultMain [
{-
        bgroup "Rational Intersection" [

             bench "Line Line Intersection" $ nf (lineIntersection
                                          (line' (r2 0 0) (r2 0 1)))
                                          (line' (r2 2345 2345) (r2 1 2))

           , bench "Seg Seg Intersection" $ nf (segIntersection
                                          (seg' (r2 0 0) (r2 1 1)))
                                          (seg' (r2 1 1) (r2 2 2))
        ]
-}

        bgroup "Linear" [
            let
                !m1 = V3 (V3 1 2 3)
                         (V3 6 4 5)
                         (V3 7 8 9)

                !m2 = V3 (V3 7 2 7)
                         (V3 6 7 5)
                         (V3 7 8 (7 :: Double))

            in bench "Matrix multiplication"     $ nf ((!*!) m1) m2

            , let
                !m1 = MMM 1 2 3
                          6 4 5
                          7 8 9

                !m2 = MMM 7 2 7
                          6 7 5
                          7 8 7

            in bench "Matrix multiplication"     $ nf (matrixMult m1) m2
        ]

        , bgroup "Geometry" [
            let
                circle = Circle' (V2 3.0 10.0) (3.0 :: Double)
                rays = [
                    -- Miss
                    Ray' (V2 0.0 0.0) (V2 (-1.0) 1.0),

                    -- Miss (one hit behind the ray).
                    Ray' (V2 0.0 0.0) (V2 0.0 (-1.0)),

                    -- Miss (two hits behind the ray).
                    Ray' (V2 0.0 0.0) (V2 (-3.0) (-10.0)),

                    -- One hit
                    Ray' (V2 0.0 0.0) (V2 0.0 1.0),

                    -- Two hit (inside)
                    Ray' (V2 4.0 9.0) (V2 (-1.0) (-2.0)),

                    -- Two hit
                    Ray' (V2 0.0 0.0) (V2 3.0 10.0),

                    -- Two hit
                    Ray' (V2 0.0 0.0) (V2 4.0 9.0)
                    ]
            in bench "ray cast circle x7" $ nf (\(rs :: [Ray Double]) -> length [
                    () | ray <- take 7000000 $ cycle rs, isJust (rayCast ray circle)
                ]) rays,

            let
                circle = Circle' (V2 3.0 10.0) (3.0 :: Double)
                ray = Ray' (V2 0.0 0.0) (V2 3.0 10.0)
            in bench "ray cast circle x1" $ nf (\(ray :: Ray Double) -> rayCast ray circle) ray,

            let
                circleX = 3.0
                circleY = 10.0
                circleR = 3.0
                rayPX = 0.0
                rayPY = 0.0
                rayDirX = 3.0
                rayDirY = 10.0
            in bench "ray cast RAW circle x1" $ nf
                (rayCastRaw rayPX rayPY rayDirX rayDirY circleX circleY) circleR
        ]

        -- bgroup "Double Line Intersection" [
        --     let
        --         !l1 = line' (V2    0    0) (V2 0  1)
        --         !l2 = line' (V2 1 1) (V2 1 (0 :: Double))
        --     in bench "Intersection"     $ nf (lineIntersection l1) l2
        --
        --   , bench "No Intersection"  $ nf (lineIntersection
        --                                   (line' (V2 0 0) (V2 1 1)))
        --                                   (line' (V2 2345 2345) (V2 1 (1 :: Double)))
        -- ]
        --
        -- , bgroup "Shadow Front" [
        --
        --             shadowFrontCircSegsN 10,
        --             shadowFrontCircSegsN 100,
        --             shadowFrontCircSegsN 1000,
        --             shadowFrontCircSegsN 10000
        --
        -- ]

        -- , bgroup "Shadow Front (Vector)" [
        --
        --             shadowFrontVCircSegsN 10,
        --             shadowFrontVCircSegsN 100,
        --             shadowFrontVCircSegsN 1000,
        --             shadowFrontVCircSegsN 10000
        --
        -- ]
    ]

shadowFrontCircSegsN :: Int -> Benchmark
shadowFrontCircSegsN n = bench (show n ++ " circular segments") $ let
    segs :: [(Int, Seg Double)]
    segs = zip [1..] [seg' (V2 (sin t1) (cos t1)) (V2 (sin t2) (cos t2))
                        | i <- [1..n]
                        , let t1 = fromIntegral i
                        , let t2 = t1 + 1]
    in segs `deepseq` nf (toShadowFront zero Nothing) segs

-- shadowFrontVCircSegsN :: Int -> Benchmark
-- shadowFrontVCircSegsN n = bench (show n ++ " circular segments") $ let
--     segs :: V.Vector (Int, Seg Double)
--     segs = V.fromList $ zip [1..] [seg' (V2 (sin t1) (cos t1)) (V2 (sin t2) (cos t2))
--                         | i <- [1..n]
--                         , let t1 = fromIntegral i
--                         , let t2 = t1 + 1]
--     in segs `deepseq` nf (toShadowFrontV zero Nothing) segs

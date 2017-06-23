{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Geometry.BVH (
        BVH,
        toBVH,
        _bvh_tests
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.List (sortOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Foldable (toList)
import Control.Monad (when)
import Control.Monad.ST
import Linear

import Geometry.Geometry

import           Test.Tasty
-- import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

data BVH p a
    = BVHEmpty
    | BVHLeaf (AABB p) (V.Vector a)
    | BVH (AABB p) (BVH p a) (BVH p a)

maxDepth :: Int
maxDepth = 50

minLeafSize :: Int
minLeafSize = 15

toBVH :: forall t p a. (Foldable t, HasAABB p a, Fractional p, Ord p) =>
    t a
    -> BVH p a
toBVH
    = toBVH' 0
    . V.fromList
    . map (\a -> let box = getAABB a in ((a, centerAABB box), box))
    . toList where
        toBVH' :: Int ->  V.Vector ((a, V2 p), AABB p) -> BVH p a
        toBVH' depth bs
            -- // From Spinfire
            -- public BoundingVolumeHierarchy(List<ItemAndBounds<T>> itemsAndBounds, int maxDepth = -1, int minLeafSize = 15)
            -- {
            --     // TODO: do experiments to determine good default values for maxDepth and minLeafSize.
            --     // Empty case.
            --     if (itemsAndBounds.Count == 0)
            --     {
            --         Bounds = null;
            --         _items = new List<T>();
            --         _subtrees = null;
            --         return;
            --     }
            = case getAABBMay . fmap snd $ bs of
                Nothing             -> BVHEmpty
                Just currentAABB    -> let
                    --     Bounds = itemsAndBounds.Select(tb => tb.Bounds).Aggregate((a, b) => a.Union(b));
                    --
                    --     if (maxDepth == 0 || itemsAndBounds.Count == 1)
                    --     {
                    --         // Leaf.
                    --         _items = itemsAndBounds.Select(p => p.Item).ToList();
                    --         _subtrees = null;
                    --         return;
                    --     }
                    --
                    --     int largerAxis = Bounds.LargerAxis();
                    V2 dx dy = diagonalAABB currentAABB
                    getComponent = if dx > dy
                        then (\(V2 x _) -> x)
                        else (\(V2 _ y) -> y)

                    --     int mid = itemsAndBounds.Median((p1, p2) => p1.Centroid[largerAxis].CompareTo(p2.Centroid[largerAxis]));
                    --     mid += 1;
                    (lo, hi) = splitMeanBy (getComponent . snd . fst) bs

                    --
                    --     // Check the minimum leaf size, front and back will have at least one element.
                    --     if (mid < minLeafSize || itemsAndBounds.Count - mid < minLeafSize)
                    --     {
                    --         // Leaf.
                    --         _items = itemsAndBounds.Select(p => p.Item).ToList();
                    --         _subtrees = null;
                    --     }
                    --     else
                    --     {
                    --         List<ItemAndBounds<T>> front = itemsAndBounds.Take(mid).ToList();
                    --         List<ItemAndBounds<T>> back = itemsAndBounds.Skip(mid).ToList();
                    --
                    --         // Node.
                    --         _items = null;
                    --         _subtrees = new[]
                    --         {
                    --             new BoundingVolumeHierarchy<T>(back, maxDepth - 1, minLeafSize),
                    --             new BoundingVolumeHierarchy<T>(front, maxDepth - 1, minLeafSize)
                    --         };
                    --     }
                    -- }
                    asLeaf = BVHLeaf currentAABB (fmap (fst .fst) bs)

                    in if depth > maxDepth || V.length lo < minLeafSize || V.length hi < minLeafSize
                        then asLeaf
                        else BVH currentAABB (toBVH' (depth + 1) lo) (toBVH' (depth + 1) hi)

instance (Floating p, Ord p, RayCast p a) => RayCastMany p (BVH p a) a where
    rayCastMany _ BVHEmpty = []
    rayCastMany ray@(Ray' rayStart _) (BVHLeaf box elems)
        | ray `intersects` box
                = sortOn (distSq rayStart . fst)
                . mapMaybe (\e -> (, e) <$> rayCast ray e)
                . V.toList
                $ elems
        | otherwise = []
    rayCastMany ray@(Ray' rayStart _) bvh = mapMaybe getHit $ rayCastMany' bvh-- TODO lazy implementeation
        where
            getHit (Left _) = Nothing
            getHit (Right (_, hit)) = Just hit

            -- type ClearanceRayHit p a = Either p (p, (V2 p, a))

            -- rayCastMany' :: BVH p a -> [ClearanceRayHit  p a]
            rayCastMany' BVHEmpty = []
            rayCastMany' (BVHLeaf box elems)
                = fromMaybe [] $ do
                    boxPoint <- rayCast ray box
                    let clearance = Left (dist boxPoint rayStart)
                    let intersections
                            = fmap Right
                            . sortOn fst
                            . mapMaybe (\e -> do
                                hitPoint <- rayCast ray e
                                return (dist rayStart hitPoint, (hitPoint, e)))
                            . V.toList
                            $ elems
                    return $ clearance : intersections
            rayCastMany' (BVH box a b)
                = fromMaybe [] $ do
                    boxPoint <- rayCast ray box
                    let clearance = Left (dist boxPoint rayStart)
                    let intersectionsA = rayCastMany' a
                    let intersectionsB = rayCastMany' b
                    return $ clearance : merge intersectionsA intersectionsB

            -- merge :: [ClearanceRayHit  p a] -> [ClearanceRayHit  p a] -> [ClearanceRayHit  p a]
            merge as [] = as
            merge [] bs = bs
            merge (a:as) (b:bs)
                | ca < cb   = a : merge as (b:bs)
                | otherwise  = b : merge (a:as) bs
                where
                    ca = toClearance a
                    cb = toClearance b
                    toClearance (Left clearance) = clearance
                    toClearance (Right (clearance, _)) = clearance

splitMeanBy :: forall b c. Ord c =>
    (b -> c)
    -- ^ getter function to get the value to sort on.
    -> V.Vector b
    -- ^ The input vector.
    -> (V.Vector b, V.Vector b)
    -- ^ partitions (u, v) where u has (length input `div` 2) length and v has (length input + 1 `div` 2) length
splitMeanBy get xsList = runST $ do
    let len = V.length xsList
    xsM <- V.thaw xsList
    let n = ((len + 1) `div` 2)
    splitN n 0 (len - 1) xsM
    xs <- V.freeze xsM
    let lo = V.take n xs
    let hi = V.drop n xs
    return (lo, hi)
    where
        -- | Rearange in place, so that the n first elements are the n smallest elements.
        -- 0 <= lo <= hi < length xs   &&   lo <= n <= hi
        splitN :: Int -> Int -> Int -> VM.MVector s b -> ST s ()
        splitN n lo hi xs = when (lo < hi) $ do
            -- Pivot
            pivot <- get <$> VM.unsafeRead xs lo
            let
                -- partition elements in [i, j] returns index of last lo value
                -- Return value mid is   i
                doPartition i j = do
                    let iLoop i' = do
                            let i'' = i' + 1
                            gi'' <- get <$> VM.read xs i''
                            if gi'' < pivot
                                then iLoop i''
                                else return i''
                    iShifted <- iLoop i

                    let jLoop j' = do
                            let j'' = j' - 1
                            gj'' <- get <$> VM.read xs j''
                            if gj'' > pivot
                                then jLoop j''
                                else return j''
                    jShifted <- jLoop j

                    if iShifted >= jShifted
                        then return jShifted
                        else do
                            VM.swap xs iShifted jShifted
                            doPartition iShifted jShifted

            -- partition
            mid <- doPartition (lo - 1) (hi + 1)

            -- if mid == n then we are done, else recurse.
            let sizeLeft = mid + 1
            when (sizeLeft /= n) $ if n < sizeLeft
                then splitN n lo mid xs
                else splitN n (mid + 1) hi xs

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary

_bvh_tests :: TestTree
_bvh_tests = testGroup "BVH internal" [

            testProperty "splitMeanBy: result lo size"
                (\(v :: V.Vector Int)
                    -> let (lo, _) = splitMeanBy id v
                        in V.length lo === (V.length v + 1) `div` 2)

            , testProperty "splitMeanBy: result hi size"
                (\(v :: V.Vector Int)
                    -> let (_, hi) = splitMeanBy id v
                        in V.length hi === V.length v `div` 2)

            , testProperty "splitMeanBy: all left elements <= all right elements"
                (\(v :: V.Vector Int)
                    -> let (lo, hi) = splitMeanBy id v
                        in V.all (\loEl -> V.all (loEl <=) hi) lo)

            -- TODO MORE TESTS!!!! model test againt bruit force ray casting

    ]

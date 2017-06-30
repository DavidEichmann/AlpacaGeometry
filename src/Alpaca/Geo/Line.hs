{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Alpaca.Geo.Line (
      Line (..)
    , LineIntersectLine (..)
) where

import           Alpaca.Geo.Classes
import           Alpaca.Geo.Dir
import           Alpaca.Geo.P2
import           Alpaca.Geo.V2

-- |A line.
data Line = Line P2 Dir

instance Prim Line where
    p ∈ Line lp ld = (lp .- p) × dirToV2 ld == 0

data LineIntersectLine
    = LILNothing
    | LILPoint P2
    | LILLine Line

instance Line :∩ Line where
    type Line ∩ Line = LineIntersectLine
    l1@(Line p1 d1) ∩ l2 = case intersectionTime l1 l2 of
        LILTNothing  -> LILNothing
        LILTPoint t1 -> LILPoint (p1 .+ (t1 .* toV2 d1))
        LILTLine     -> LILLine l1

data LineIntersectLineTime
    = LILTNothing
    | LILTPoint Double
    | LILTLine

intersectionTime :: Line -> Line -> LineIntersectLineTime
intersectionTime (Line p1 d1) (Line p2 d2)
    | d2xd1 /= 0  = LILTPoint (p21xd2 / d2xd1)
    | p21xd2 == 0 = LILTLine
    | otherwise   = LILTNothing
    where
        d2xd1 = d2 × d1
        p21xd2 = (p1 .- p2) × d2

{-
instance (Fractional p, Eq p) => Eq Line where
    l1 == l2 = case lineIntersectionT l1 l2 of
                LTLine -> True
                _      -> False

data LineIntersectT p
    = LTLine
    | LTPoint {-# UNPACK #-} p {-# UNPACK #-} p
    | LTNothing
    deriving (Show, Read, Eq)

data LineIntersect p
    = LLine Line
    | LPoint (Pos p)
    | LNothing
    deriving (Show, Read, Eq, Generic, NFData)

data LineRayIntersect p
    = LRRay (Ray p)
    | LRPoint (Pos p)
    | LRNothing
    deriving (Show, Read, Eq, Generic, NFData)

data LineSegIntersect p
    = LSSeg (Seg p)
    | LSPoint (Pos p)
    | LSNothing
    deriving (Show, Read, Eq, Generic, NFData)

data SegIntersect p
    = SSeg (Seg p)
    | SPoint (Pos p)
    | SNothing
    deriving (Show, Read, Eq, Generic, NFData)

data SegRayIntersect p
    = SRSeg (Seg p)
    | SRPoint (Pos p)
    | SRNothing
    deriving (Show, Read, Eq, Generic, NFData)


centerAABB :: Fractional p => AABB p -> Pos p
centerAABB (AABB' a b) = (a + b) * 0.5

scaleAABB :: Fractional p => p -> AABB p -> AABB p
scaleAABB s' box@(AABB' a b) = AABB' (c + (s *^ (a - c))) (c + (s *^ (b - c)))
    where
        s = abs s'
        c = centerAABB box

unionAABB :: Ord p => AABB p -> AABB p -> AABB p
unionAABB
    (AABB' (V2 loX1 loY1) (V2 hiX1 hiY1))
    (AABB' (V2 loX2 loY2) (V2 hiX2 hiY2))
        = AABB'
            (V2 (min loX1 loX2) (min loY1 loY2))
            (V2 (max hiX1 hiX2) (max hiY1 hiY2))

diagonalAABB :: Num p => AABB p -> Vec p
diagonalAABB (AABB' lo hi) = hi - lo

{-# SPECIALIZE lineIntersectionT :: Line Double -> Line Double -> LineIntersectT Double #-}
lineIntersectionT   :: (Fractional a, Eq a)
                    => Line a           -- ^ First line
                    -> Line a           -- ^ Second line
                    -> LineIntersectT a -- ^ Line intersection stuff
lineIntersectionT (Line' p1 d1) (Line' p2 d2)
    = if d2Xd1 == 0
        then if p21Xd2 == 0
            then LTLine
            else LTNothing
        else let
            t1 = p21Xd2 / d2Xd1
            t2 = (p21 `crossZ` d1) / d2Xd1
            in LTPoint t1 t2
    where
        p21 = p1 - p2
        d2Xd1 = d2 `crossZ` d1
        p21Xd2 = p21 `crossZ` d2

{-# INLINEABLE lineIntersection #-}
-- {-# SPECIALIZE lineIntersection :: Line Double -> Line Double -> LineIntersect Double #-}
-- {-# SPECIALIZE lineIntersection :: Line Float -> Line Float -> LineIntersect Float #-}
-- {-# INLINABLE lineIntersection #-}
lineIntersection    :: (Fractional a, Eq a)
                    => Line a
                    -> Line a
                    -> LineIntersect a
lineIntersection l1@(Line' p1 d1) l2 =
    case lineIntersectionT l1 l2 of
        LTLine       -> LLine l1
        LTPoint t1 _ -> LPoint (p1 + (d1 ^* t1))
        LTNothing    -> LNothing

{-# SPECIALIZE lineRayIntersection :: Line Double -> Ray Double -> LineRayIntersect Double #-}
lineRayIntersection :: (Fractional a, Ord a)
                    => Line a
                    -> Ray a
                    -> LineRayIntersect a
lineRayIntersection line ray@(Ray' rayP rayD) =
    case lineIntersectionT line (Line' rayP rayD) of
        LTLine         -> LRRay ray
        LTPoint _ rayT -> if 0 <= rayT
                            then LRPoint (rayP + (rayD ^* rayT))
                            else LRNothing
        LTNothing      -> LRNothing

{-# SPECIALIZE lineSegIntersection :: Line Double -> Seg Double -> LineSegIntersect Double #-}
lineSegIntersection :: (Fractional a, Ord a)
                    => Line a
                    -> Seg a
                    -> LineSegIntersect a
lineSegIntersection line seg@(Seg' p1 p2) =
    case lineIntersectionT line (Line' p1 p12) of
        LTLine         -> LSSeg seg
        LTPoint _ segT -> if 0 <= segT && segT <= 1
                            then LSPoint (p1 + (p12 ^* segT))
                            else LSNothing
        LTNothing      -> LSNothing
    where
        p12 = p2 - p1

data AorB = A | B deriving (Show, Eq)

{-# SPECIALIZE segIntersection :: Seg Double -> Seg Double -> SegIntersect Double #-}
segIntersection :: forall a. (Fractional a, Ord a)
                => Seg a
                -> Seg a
                -> SegIntersect a
segIntersection segA@(Seg' a1 a2) segB@(Seg' b1 b2)
    | a1 == a2  = maybe SNothing SPoint $ pointSegIntersection a1 segB
    | b1 == b2  = maybe SNothing SPoint $ pointSegIntersection b1 segA
    | otherwise = case lineIntersectionT (Line' a1 a12) (Line' b1 b12) of
        LTLine        -> if areIntersecting
                            then if p1 == p2
                                then SPoint p1
                                else SSeg (Seg' p1 p2)
                            else SNothing
                        where
                            -- order points along the line
                            _orderedPoints :: [(AorB, Pos a)]
                            _orderedPoints@((s0, _) : (s1, p1) : (_, p2) : _) = sortOn (dot a12 . snd) [(A, a1), (B, b1), (A, a2), (B, b2)]
                            areIntersecting = s0 /= s1

        LTPoint aT bT -> if 0 <= aT && aT <= 1 && 0 <= bT && bT <= 1
                            then SPoint (a1 + (a12 ^* aT))
                            else SNothing
        LTNothing     -> SNothing
    where
        a12 = a2 - a1
        b12 = b2 - b1

{-# SPECIALIZE segRayIntersection :: Seg Double -> Ray Double -> SegRayIntersect Double #-}
segRayIntersection :: (Fractional a, Ord a)
                   => Seg a
                   -> Ray a
                   -> SegRayIntersect a
segRayIntersection seg (Ray' rayP rayD) =
    case lineSegIntersection (Line' rayP rayD) seg of
        LSSeg (Seg' a b) ->
            if (a - rayP) `dot` rayD < 0
            then
                if (b - rayP) `dot` rayD < 0
                then SRNothing
                else
                    if rayP == b
                    then SRPoint b
                    else SRSeg (Seg' rayP b)
            else
                if (b - rayP) `dot` rayD < 0
                then
                    if rayP == a
                    then SRPoint a
                    else SRSeg (Seg' rayP a)
                else SRSeg seg

        LSPoint p ->
            if (p - rayP) `dot` rayD < 0
            then SRNothing
            else SRPoint p

        LSNothing -> SRNothing

pointSegIntersection :: (Ord p, Num p) => Pos p -> Seg p -> Maybe (Pos p)
pointSegIntersection p (Seg' a b)
    | p == a ||
      p == b ||
      (pa `crossZ` pb == 0 && pa `dot` pb < 0)
                = Just p
    | otherwise = Nothing
    where
        pa = a - p
        pb = b - p

pointLineIntersectionT :: (Eq p, Fractional p) => Pos p -> Line p -> Maybe p
pointLineIntersectionT p (Line' x d)
    | p == x ||
      xp `crossZ` d == 0
                = Just $ xp `dot` d / quadrance d
    | otherwise = Nothing
    where
        xp = p - x

pointInside :: forall p. (Fractional p, Ord p) => Pos p -> Polygon p -> Bool
pointInside (V2 x y) (Polygon' shape) = foldl' (/=) False . map intersectsEdge . zip shape $ rotate 1 shape
    where
        intersectsEdge :: (Pos p, Pos p) -> Bool
        intersectsEdge (V2 ax ay, V2 bx by)
            = (ay > y) /= (by > y)
            && x < (bx - ax) * (y - ay) / (by - ay) + ax

--
-- Minimum Distance
--

projectPointOnLineT :: Fractional p => Pos p -> Line p -> p
projectPointOnLineT p (Line' q d) = ((p - q) `dot` d) / (d `dot` d)

projectPointOnLine :: Fractional p => Pos p -> Line p -> Pos p
projectPointOnLine p l@(Line' q d) = (t *^ d) + q
    where
        t = projectPointOnLineT p l

projectPointOnSeg :: (Fractional p, Ord p) => Pos p -> Seg p -> Pos p
projectPointOnSeg p s@(Seg' a b)
    | t < 0 = a
    | t > 1 = b
    | otherwise = ((1 - t) *^ a) + (t *^ b)
    where
        t = projectPointOnLineT p (toLine s)

-- -- Some Geometry
-- data AABB = AABB Pos a Pos a

-- aabb :: [Pos a] -> AABB
-- aabb []     = error "Trying to calculate AABB of an empty set."
-- aabb (p:ps) = foldl accumAABB (AABB p p) ps
--     where
--         accumAABB (AABB (V2 minX minY) (V2 maxX maxY)) (V2 x y)
--             = AABB
--                 (V2 (min minX x) (min minY y))
--                 (V2 (max maxX x) (max maxY y))



-- epsilon = 1e-10

-- data ClipPos a = In Pos a | Out Pos a

-- clipLinePath :: Pos a         -- ^ clipping line point
--              -> Vec a         -- ^ clipping line dir (right of this dir will be clipped)
--              -> [Pos a]       -- ^ path to clip
--              -> [Pos a]       -- ^ clipped path
-- clipLinePath _ _ [] = []
-- clipLinePath lP lDir path = clippedPath
--     where

--         inOutPath = map toClipPos a path

--         edges = zip inOutPath (tail inOutPath)

--         clippedPath = case head inOutPath of
--             In  startP -> startP : clippedPath'
--             Out _      -> clippedPath'

--         clippedPath' = concatMap edgeContribution edges

--         edgeContribution :: (ClipPos a, ClipPos a) -> [Pos a]
--         edgeContribution ( In _,  In b)   = [b]
--         edgeContribution (Out _, Out _)   = []
--         edgeContribution ( In a, Out b)   = [fromJust $ lineIntersection lP lDir a (b - a)]
--         edgeContribution (Out a,  In b)   = [fromJust $ lineIntersection lP lDir a (b - a), b]

--         toClipPos a :: Pos a -> ClipPos a
--         toClipPos a p = if lDir `crossZ` (p - lP) > 0
--             then In p
--             else Out p


-- clipConcavePolygonPath :: [Pos a]    -- ^ clipping concave polygon
--                        -> [Pos a]    -- ^ path to clip
--                        -> [Pos a]    -- ^ clipped line
-- clipConcavePolygonPath clipPolygon path = foldl (\p (a, ab) -> clipLinePath a ab p) path clipLines
--     where
--         clipLines = map (\(a, b) -> (a, (b - a))) $ zip clipPolygon (tail $ cycle clipPolygon)


--
--
--  COOL NEW CLASSES
--
--


--
-- HasAABB and HasAABBMay
--

-- type family BaseType a where
--     BaseType (V2 p) = p
--     BaseType Line = p
--     BaseType (Ray p) = p
--     BaseType (Seg p) = p
--     BaseType (AABB p) = p
--     BaseType (Polygon p) = p
--     BaseType (t a) = BaseType a
--     BaseType p = p

-- class BaseTypeClass a where type BaseTypeC a :: *
-- instance (Foldable t, BaseTypeClass a) => BaseTypeClass (t a) where type BaseTypeC (t a) = BaseType a

-- type family p where
--     Base (V2 p)       = p
--     Base Line     = p
--     Base (Ray p)      = p
--     Base (Seg p)      = p
--     Base (AABB p)     = p
--     Base (Polygon p)  = p
--     Base (t p)        = Base p
--     Base p            = p

class HasAABB p a | a -> p where
    getAABB :: a -> AABB p
class HasAABBMay p a | a -> p where
    getAABBMay :: a -> Maybe (AABB p)

instance HasAABB p (AABB p) where
    getAABB = id
instance Ord p => HasAABB p (Seg p) where
    getAABB (Seg' (V2 x1 y1) (V2 x2 y2))
        = AABB'
            (V2 (min x1 x2) (min y1 y2))
            (V2 (max x1 x2) (max y1 y2))
-- instance HasAABB a => HasAABBMay a where
--     getAABBMay = Just . getAABB

-- instance {-# OVERLAPPING #-} HasAABBMay p (AABB p) where
--     getAABBMay = Just

instance {-# OVERLAPPABLE #-} (Foldable t, Ord p, HasAABB p a) => HasAABBMay p (t a) where
    getAABBMay as = case fmap getAABB (toList as) of
        [] -> Nothing
        bs -> Just $ foldl1' unionAABB bs

instance {-# OVERLAPPING #-} (Foldable t, Ord p) => HasAABBMay p (t (V2 p)) where
    getAABBMay = aabbFromPoints' . toList
        where
            aabbFromPoints' []      = Nothing
            aabbFromPoints' (p:ps)  = Just (foldl'
                                    (\(AABB' (V2 loX loY) (V2 hiX hiY)) (V2 x y) -> AABB'
                                        (V2 (min x loX) (min y loY))
                                        (V2 (max x hiX) (max y hiY)))
                                    (AABB' p p)
                                    ps)

-- x :: p -> Maybe (AABB p)
-- x p= getAABBMay [AABB' (V2 1.0 1.0) (V2 2.0 3.0), AABB' (V2 1.0 1.0) (V2 2.0 p)]

--
-- Distance
--

class Distance p a b | a b -> p where
    dist :: a -> b -> p
    distSq :: a -> b -> p

instance Floating p => Distance p (Pos p) (Pos p) where
    dist = distance
    distSq = qd

--
-- Ray Cast
--

class Fractional p => RayCast p a | a -> p where
    rayCast :: Ray p -> a -> Maybe (Pos p)
    rayCast ray = rayInvCast (toRayInv ray)

    rayInvCast :: RayInv p -> a -> Maybe (Pos p)
    rayInvCast rayInv = rayCast (fromRayInv rayInv)

{-# INLINE quadrance' #-}
quadrance' :: Double -> Double -> Double
quadrance' x y = (x * x) + (y * y)

{-# INLINE dot' #-}
dot' :: Double -> Double -> Double -> Double -> Double
dot' x1 y1 x2 y2 = (x1 * x2) + (y1 * y2)

{-# INLINE at_t' #-}
at_t' :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
at_t' posX posY dirX dirY t = (posX + (dirX * t), posY + (dirY * t))

rayCastRaw :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Maybe (Double, Double)
rayCastRaw pX pY dirX dirY centerX centerY radius = let
    p_to_cX = centerX - pX
    p_to_cY = centerY - pY

    -- a, b, and c of the quadratic formula.
    a = quadrance' dirX dirY
    b = -2.0 * (dot' p_to_cX p_to_cY dirX dirY)
    c = quadrance' p_to_cX p_to_cY - (radius * radius)

    u = b * b - 4.0 * a * c
    in if u < 0.0
        then Nothing
        else if u == 0.0
            then let t = -b / (2.0 * a) in
                if t >= 0.0
                    then Just (at_t' pX pY dirX dirY t)
                    else Nothing
        else let
                v = sqrt u
                denom = 2.0 * a

                -- Note that a is always positive so lo and high are clear.
                lo_t = ((-b) - v) / denom
            in if lo_t >= 0.0
                then Just (at_t' pX pY dirX dirY lo_t)
                else let hi_t = ((-b) + v) / denom in if hi_t >= 0.0
                    then
                        -- Ray starts inside the circle.
                        -- This is a filled circle hence use ray start.
                        Just (pX, pY)
                    else Nothing

{-# SPECIALIZE rayCast :: Ray Double -> Circle Double -> Maybe (Pos Double)  #-}
instance (Floating p, Ord p) => RayCast p (Circle p) where
    rayCast ray@(Ray' p dir) (Circle' center radius) = let
        p_to_c = center - p

        -- a, b, and c of the quadratic formula.
        a = quadrance dir
        b = -2.0 * (p_to_c `dot` dir)
        c = quadrance p_to_c - (radius * radius)

        u = b * b - 4.0 * a * c
        in if u < 0.0
            then Nothing
            else if u == 0.0
                then let t = -b / (2.0 * a) in
                    if t >= 0.0
                        then Just (ray `at_t` t)
                        else Nothing
            else let
                    v = sqrt u
                    denom = 2.0 * a

                    -- Note that a is always positive so lo and high are clear.
                    lo_t = ((-b) - v) / denom
                in if lo_t >= 0.0
                    then Just (ray `at_t` lo_t)
                    else let hi_t = ((-b) + v) / denom in if hi_t >= 0.0
                        then
                            -- Ray starts inside the circle.
                            -- This is a filled circle hence use ray start.
                            Just (p)
                        else Nothing

instance (Fractional p, Ord p) => RayCast p (AABB p) where
    rayCast
        ray @ (Ray' start dir)
        (AABB' (V2 minX minY) (V2 maxX maxY)) =
            -- https://tavianator.com/fast-branchless-raybounding-box-intersections/
            let
                RayInv (V2 x y) (V2 invDirX invDirY) = toRayInv ray
                tx1 = (minX - x) * invDirX
                tx2 = (maxX - x) * invDirX
                ty1 = (minY - y) * invDirY
                ty2 = (maxY - y) * invDirY
                tmin = max (min tx1 tx2) (min ty1 ty2)
                tmax = min (max tx1 tx2) (max ty1 ty2)
            in
                if tmax >= tmin
                    then Just $ start + (dir ^* tmin)
                    else Nothing

instance (Floating p, Ord p) => RayCast p (Seg p) where
    rayCast ray@(Ray' start _) seg = case segRayIntersection seg ray of
        SRSeg (Seg' a b)    -> Just $ if distSq start a < distSq start b
                                        then a
                                        else b
        SRPoint p           -> Just p
        SRNothing           -> Nothing

class RayCastMany p a b | p a -> b where
    rayCastMany :: Ray p -> a -> [(Pos p, b)]

--
-- Intersection test
--

class Intersects a b where
    intersects :: a -> b -> Bool

instance (Fractional p, Ord p) => Intersects (Ray p) (AABB p) where
    intersects r b = intersects (toRayInv r) b
instance (Fractional p, Ord p) => Intersects (AABB p) (Ray p) where
    intersects = flip intersects

instance (Num p, Ord p) => Intersects (RayInv p) (AABB p) where
    intersects
        (RayInv (V2 x y) (V2 invDirX invDirY))
        (AABB' (V2 minX minY) (V2 maxX maxY)) =
            -- https://tavianator.com/fast-branchless-raybounding-box-intersections/
            let
                tx1 = (minX - x) * invDirX
                tx2 = (maxX - x) * invDirX
                ty1 = (minY - y) * invDirY
                ty2 = (maxY - y) * invDirY
                tmin = max (min tx1 tx2) (min ty1 ty2)
                tmax = min (max tx1 tx2) (max ty1 ty2)
            in
                tmax >= tmin
instance (Num p, Ord p) => Intersects (AABB p) (RayInv p) where
    intersects = flip intersects

data RayInv p
    = RayInv
        (Pos p)
        -- ^ The ray start.
        (Vec p)
        -- ^ The ray inverse direction vector (V2 (1/directionX) (1/directionY))

toRayInv :: Fractional p => Ray p -> RayInv p
toRayInv (Ray' start (V2 dirX dirY)) = RayInv start (V2 (1 / dirX) (1 / dirY))

fromRayInv :: Fractional p => RayInv p -> Ray p
fromRayInv (RayInv start (V2 dirInvX dirInvY)) = Ray' start (V2 (1 / dirInvX) (1 / dirInvY))
-}

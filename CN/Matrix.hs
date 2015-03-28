-- adopted from https://github.com/laughedelic/sparse-lin-alg
module Matrix
where

import Data.Array.Unboxed
import Data.Array.Base as A ((!))
-- import Data.Foldable as F
import Data.List as L

import Vector

type BMx a  = BVec (BVec a)

data BandMatrix a = BM
    { mx :: BMx a } deriving Eq

instance Functor BandMatrix where
    fmap f m    = m { mx = amap (amap f) $ mx m }

instance (Eq a, Num a) => Num (BandMatrix a) where
    -- TODO: +, -
    (+) = undefined
    (*) = undefined
    negate          = fmap negate
    -- fromInteger 0  = emptyMx
    -- fromInteger x  = diagonalMx [fromInteger x]
    fromInteger     = undefined
    abs             = fmap abs
    signum          = fmap signum

-- TODO: Monoid

instance (Show a, Num a, Eq a) => Show (BandMatrix a) where
    show = unlines . map show . fillMx

height, width, dim, ubk, obk :: BandMatrix a -> BKey
height m = length $ elems $ mx m A.! 0
width    = height
dim      = height
ubk     = abs . minimum . indices . mx
obk     = abs . maximum . indices . mx

-- TODO: setSize

emptyMx :: BandMatrix a
emptyMx = BM $ array (1,0) []

zeroMx :: (Num a) => Int -> BandMatrix a
zeroMx n = BM $ listArray (0,0) [listArray (1,n) $ repeat 0]

isZeroMx, isNotZeroMx :: (Num a, Eq a) => BandMatrix a -> Bool
isZeroMx    = L.all (L.all (==0) . elems) . elems . mx
isNotZeroMx = not . isZeroMx

idMx :: (Num a) => Int -> BandMatrix a
idMx n = BM $ listArray (0,0) [listArray (1,n) $ repeat 1]

-- TODO: (//), hconcat, vconcat, sizedBlockMx, sizedBlockSMx, blockMx, blockSMx

-- TODO: adding, deleting rows/cols

-- TODO: partitionMx, separateMx

(#) :: Num a => BandMatrix a -> (BKey,BKey) -> a
m # (r,c)
    | dia > err = error "Out of bound"
    | dia > ul = 0
    | dia < ll = 0
    | otherwise = mx m A.! dia A.! e
    where   dia = c - r
            e   = max r c - abs dia
            ul  = maximum $ indices $ mx m
            ll  = minimum $ indices $ mx m
            err = length (elems $ mx m A.! 0) - 1

diag :: BandMatrix a -> BKey -> BandVector a
diag m i = BV $ mx m A.! i

row :: Num a => BandMatrix a -> BKey -> BandVector a
row m i = bandList $ map (\j -> m # (i,j)) [1..n]
    where   n = dim m

col :: Num a => BandMatrix a -> BKey -> BandVector a
col m j = bandList $ map (\i -> m # (i,j)) [1..n]
    where   n = dim m

-- TODO: update, erase

-- TODO: rows, cols, diags

diagonalMx :: (Num a, Eq a) => [a] -> BandMatrix a
diagonalMx l = BM $ listArray (0,0) [listArray (1,n) l]
    where n = L.length l

mainDiag :: BandMatrix a -> BandVector a
mainDiag m = diag m 0

fromDiags :: Num a => BVec(BandVector a) -> BandMatrix a
fromDiags vl = BM $ listArray (ubk',obk') m
    where   n'      = vecLength $ vl A.! 0
            n''     = ubk'+obk'+1
            n       = max n'' n'
            ubk'    = minimum keys
            obk'    = maximum keys
            keys    = indices vl
            trunc' [] _ = []
            trunc' (lh:ll) i
                | i <= obk' = vec ( setLength (n-abs i) lh) : trunc' ll (i+1)
                | otherwise = []
            m       = trunc' (elems vl) ubk'

fromBand :: Num a => BKey -> (BKey,BKey) -> [a] -> BandMatrix a
fromBand n (ubk'',obk') l = BM $ listArray (ubk',obk') m
    where   buildDiag i x   = listArray (1,j) $ replicate j x
                where j = n - abs i
            buildMx [] _    = []
            buildMx (lh:ll) i
                | i <= obk' = buildDiag i lh : buildMx ll (i+1)
                | otherwise = []
            m               = buildMx l ubk'
            ubk'            = negate ubk''

fillMx :: Num a => BandMatrix a -> [[a]]
fillMx m = map (fillVec . row m) [1..n]
    where   n   = dim m

-- TODO: trans
-- TODO: mulVM

mulMV :: Num a => BandMatrix a -> BandVector a -> BandVector a
m `mulMV` v
    | dim m /= vecLength v = error "dimension error"
    | otherwise = bandList l
    where   entry' i    = sum $ L.zipWith (*) ms vs
                where   ms  = map (\j -> m # (i,j)) [a..b]
                        vs  = map (\j -> v Vector.! j) [a..b]
                        a   = max 1 (i - ubk m)
                        b   = min (dim m) (obk m + i)
            l           = L.map entry' [1..(dim m)]

mulSM :: Num a => a -> BandMatrix a -> BandMatrix a
s `mulSM` m = fmap (*s) m

mulMS :: Num a => BandMatrix a -> a -> BandMatrix a
m `mulMS` s = s `mulSM` m

(.**) :: Num a => a -> BandMatrix a -> BandMatrix a
(.**) = mulSM

(**.):: Num a => BandMatrix a -> a -> BandMatrix a
(**.) = mulMS

-- TODO
-- mul :: Num a => BandMatrix a -> BandMatrix a -> BandMatrix a
-- mul m1 m2
--     | n /= dim m2  = error "dimension error"
--     | otherwise = fromDiags m3
--     where   m3          = listArray (ubk',obk') arrs
--             elem' (i,j) = sum $ L.zipWith (*)
--                     (map (\k -> m1 # (i,k)) idxs) (map (\k -> m1 # (i,k)) idxs)
--                 where   idxs    = [i0..ie]
--                         i0      = maximum [1,i-ubk1+1,j-obk2+1]
--                         ie      = minimum [n,i+obk1-1,j+ubk2-1]
--             n           = dim m1
--             ubk1        = ubk m1
--             ubk2        = ubk m2
--             obk1        = obk m1
--             obk2        = obk m2
--             ubk'        = ubk1+ubk2
--             obk'        = obk1+obk2
--             mkArr k
--                 | k > obk'  = []
--                 | otherwise = listArray (1,l)
--                     $ map elem (elem' . idx) [1..l] : mkArr (k+1)
--                 where   l       = n - abs k
--                         idx j
--                             | k < 0     = (1+abs k+j,1+j)
--                             | otherwise = (1+j,1+k)
--             arrs        = mkArr ubk'


-- adopted from https://github.com/laughedelic/sparse-lin-alg
module Vector
where

import Data.Array.Unboxed
import Data.Array.Base as A ((!))
import Data.Foldable as F
import Data.List as L

type BKey    = Int
type BVec a = Array BKey a

data BandVector a = BV
    { vec :: BVec a } deriving Eq

    -- TODO: Eq

instance Functor BandVector where
    fmap f v    = v { vec = amap f (vec v) }

instance Foldable BandVector where
    foldr f d v = F.foldr f d (vec v)

instance (Eq a, Num a) => Num (BandVector a) where
    (+)           = Vector.zipWith (+)
    -- (*)           = Vector.zipWith (*)
    (*)           = undefined
    negate        = fmap negate
    -- fromInt 0 = emptyVec
    -- fromInt x = singVec (fromInt x)
    fromInteger   = undefined
    abs           = fmap abs
    signum        = fmap signum

instance (Show a, Eq a) => Show (BandVector a) where
    show = show . fillVec

vecLength :: BandVector a -> BKey
vecLength v = u - l + 1
    where   idx = indices $ vec v
            u   = L.maximum idx
            l   = L.minimum idx

setLength :: (Num a) => BKey -> BandVector a -> BandVector a
setLength n v = v { vec = newvec }
    where   newvec  = listArray (1,n) arr
            arr     = elems (vec v) ++ repeat 0

emptyVec :: BandVector a
emptyVec = BV $ array (1,0) []

zeroVec :: Num a => BKey -> BandVector a
zeroVec n = BV $ listArray (1,n) $ repeat 0

isZeroVec, isNotZeroVec :: (Eq a,Num a) => BandVector a -> Bool
isZeroVec = L.all (==0) . elems . vec
isNotZeroVec = not . isZeroVec

singVec :: (Eq a, Num a) => a -> BandVector a
singVec x = BV $ listArray (1,1) [x]

-- TODO: partitionVec

(!) :: BandVector a -> BKey -> a
v ! i = (A.!) (vec v) i

-- TODO: eraseInVec, vecIns

zipWith :: (a -> a -> a) -> BandVector a -> BandVector a -> BandVector a
zipWith f vv@(BV v) wv@(BV w)
    | vecLength wv /= n = error "zipWith: dimension error"
    | otherwise = BV $ listArray (1,n) arr
    where   n   = vecLength vv
            va  = elems v
            wa  = elems w
            arr = L.zipWith f va wa

fillVec :: BandVector a -> [a]
fillVec = elems . vec

bandList :: [a] -> BandVector a
bandList l = BV $ listArray (1,n) l
    where   n = length l

dot :: Num a => BandVector a -> BandVector a -> a
v `dot` w   = L.sum $ elems $ vec $ Vector.zipWith (*) v w

(·) :: Num a => BandVector a -> BandVector a -> a
(·) = dot

mulSV :: Num a => a -> BandVector a -> BandVector a
mulSV s = fmap (*s)

(.*) :: Num a => a -> BandVector a -> BandVector a
(.*) = mulSV

mulVS :: Num a => BandVector a -> a -> BandVector a
v `mulVS` s = s `mulSV` v

(*.) :: Num a => BandVector a -> a -> BandVector a
(*.) = mulVS


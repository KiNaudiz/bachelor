-- adopted from https://github.com/laughedelic/sparse-lin-alg
module Vector
where

import Data.Array.Unboxed
import Data.Array.Base as A ((!))
import Data.Foldable as F
import Data.List as L

type VKey   = Int
type Vec a  = Array VKey a

data Vector a = VV
    { vec :: Vec a } deriving Eq

instance Functor Vector where
    fmap f v    = v { vec = amap f (vec v) }

instance Foldable Vector where
    foldr f d v = F.foldr f d (vec v)

instance (Eq a, Num a) => Num (Vector a) where
    (+)           = Vector.zipWith (+)
    -- (*)           = Vector.zipWith (*)
    (*)           = undefined
    negate        = fmap negate
    -- fromInt 0 = emptyVec
    -- fromInt x = singVec (fromInt x)
    fromInteger   = undefined
    abs           = fmap abs
    signum        = fmap signum

instance (Show a, Eq a) => Show (Vector a) where
    show = show . fillVec

vecLength :: Vector a -> VKey
vecLength = snd . bounds . vec

setLength :: (Num a) => VKey -> Vector a -> Vector a
setLength n v = v { vec = newvec }
    where   newvec  = listArray (1,n) arr
            arr     = elems (vec v) ++ repeat 0

emptyVec :: Vector a
emptyVec = VV $ array (1,0) []

zeroVec :: Num a => VKey -> Vector a
zeroVec n = VV $ listArray (1,n) $ repeat 0

isZeroVec, isNotZeroVec :: (Eq a,Num a) => Vector a -> Bool
isZeroVec = L.all (==0) . elems . vec
isNotZeroVec = not . isZeroVec

singVec :: (Eq a, Num a) => a -> Vector a
singVec x = VV $ listArray (1,1) [x]

-- TODO: partitionVec

(!) :: Vector a -> VKey -> a
v ! i = (A.!) (vec v) i

-- TODO: eraseInVec, vecIns

zipWith :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
zipWith f vv@(VV v) wv@(VV w)
    | vecLength wv /= n = error "zipWith: dimension error"
    | otherwise = VV $ listArray (1,n) arr
    where   n   = vecLength vv
            va  = elems v
            wa  = elems w
            arr = L.zipWith f va wa

fillVec :: Vector a -> [a]
fillVec = elems . vec

vecList :: [a] -> Vector a
vecList l = VV $ listArray (1,n) l
    where   n = length l

dot :: Num a => Vector a -> Vector a -> a
v `dot` w   = L.sum $ L.zipWith (*) (fillVec v) (fillVec w)

(·) :: Num a => Vector a -> Vector a -> a
(·) = dot

mulSV :: Num a => a -> Vector a -> Vector a
mulSV s = fmap (*s)

(.*) :: Num a => a -> Vector a -> Vector a
(.*) = mulSV

mulVS :: Num a => Vector a -> a -> Vector a
v `mulVS` s = s `mulSV` v

(*.) :: Num a => Vector a -> a -> Vector a
(*.) = mulVS


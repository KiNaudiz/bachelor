module ValueMatrix
where

import Data.Array.Unboxed
import Data.Array.Base as A ((!))
import Data.Monoid
import Control.Arrow
import Vector

type VMKey = (Int,Int)
type VMat a = Array VMKey a

data ValueMatrix a = VM
    { vmat :: VMat a } deriving Eq

instance Functor ValueMatrix where
    fmap f vm   = vm { vmat = amap f (vmat vm) }

(#) :: ValueMatrix a -> VMKey -> a
vm # key = vmat vm A.! key

rows :: ValueMatrix a -> Vector (Vector a)
rows vm = vecList cols'
    where   (xdim,ydim) = dim vm
            mkRow j i
                | j > ydim  = []
                | otherwise = vm # (i,j) : mkRow (j+1) i
            cols'       = map (vecList . mkRow 1) [1..xdim]

cols :: ValueMatrix a -> Vector (Vector a)
cols vm = vecList rows'
    where   (xdim,ydim) = dim vm
            mkCol i j
                | i > xdim  = []
                | otherwise = vm # (i,j) : mkCol (i+1) j
            rows'       = map (vecList . mkCol 1) [1..ydim]

fromRows :: Vector (Vector a) -> ValueMatrix a
fromRows vecs = VM $ listArray ((1,1),(xdim,ydim)) vals
    where   xdim    = vecLength vecs
            ydim    = vecLength $ vecs Vector.! 1
            vals    = mconcat $ map fillVec $ fillVec vecs

fromCols :: Vector (Vector a) -> ValueMatrix a
fromCols vecs = transpose $ VM $ listArray ((1,1),(ydim,xdim)) vals
    where   ydim    = vecLength vecs
            xdim    = vecLength $ vecs Vector.! 1
            vals    = mconcat $ map fillVec $ fillVec vecs

transpose :: ValueMatrix a -> ValueMatrix a
transpose vm = VM $ array ((1,1),dim vm) l
    where   l           = map (first inv') $ assocs $ vmat vm
            inv' (a,b)  = (b,a) 

dim :: ValueMatrix a -> VMKey
dim = snd . bounds . vmat

instance (Show a, Eq a) => Show (ValueMatrix a) where
    show = show . rows

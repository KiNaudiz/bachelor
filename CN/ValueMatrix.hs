module ValueMatrix
where

import Data.Array.Unboxed
import Data.Array.Base as A ((!))
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

-- TODO: improve, too slow
dim :: ValueMatrix a -> VMKey
dim vm = (xdim,ydim)
    where   keys    = indices $ vmat vm
            xdim    = maximum $ map fst keys
            ydim    = maximum $ map snd keys

instance (Show a, Eq a) => Show (ValueMatrix a) where
    show = show . rows

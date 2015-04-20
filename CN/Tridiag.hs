-- adopted from https://github.com/laughedelic/sparse-lin-alg
module Tridiag
where

import Data.Array.Unboxed
import Data.Array.Base as A ((!))
-- import Data.Foldable as F
import Data.List as L

import Vector

type TDMx a  = (Vec a,Vec a,Vec a)

mapTDMx :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTDMx f (a,b,c) = (f a,f b,f c)

data TridiagMatrix a = TDM
    { mx :: TDMx a } deriving Eq

instance Functor TridiagMatrix where
    fmap f m    = m { mx = mapTDMx (amap f) $ mx m }

instance (Eq a, Num a) => Num (TridiagMatrix a) where
    (TDM (a,b,c)) + (TDM (d,e,f))
        | n /= L.length e'  = error "(+): dimension error"
        | otherwise  =
            TDM (listArray (1,n-1) (L.zipWith (+) a' d')
                ,listArray (1,n) (L.zipWith (+) b' e')
                ,listArray (1,n-1) (L.zipWith (+) c' f'))
        where   [a',b',c',d',e',f']  = map elems [a,b,c,d,e,f]
                n   = L.length b'
    (*) = undefined
    negate          = fmap negate
    -- fromInteger 0  = emptyMx
    -- fromInteger x  = diagonalMx [fromInteger x]
    fromInteger     = undefined
    abs             = fmap abs
    signum          = fmap signum

-- TODO: Monoid

instance (Show a, Num a, Eq a) => Show (TridiagMatrix a) where
    show = unlines . map show . fillMx

height, width, dim, ubk, obk :: TridiagMatrix a -> VKey
height      = vecLength . (-!- 0)
width       = height
dim         = height
ubk _       = 1
obk _       = 1

-- TODO: setSize

emptyMx :: Num a => TridiagMatrix a
emptyMx = TDM (listArray (1,0) [],listArray (1,0) [],listArray (1,0) [])

zeroMx :: (Num a) => Int -> TridiagMatrix a
zeroMx n = fromBand n (0,0,0)

isZeroMx, isNotZeroMx :: (Num a, Eq a) => TridiagMatrix a -> Bool
isZeroMx m  = mapTDMx (L.all (==0) . elems) (mx m) == (True,True,True)
isNotZeroMx = not . isZeroMx

idMx :: (Num a) => Int -> TridiagMatrix a
idMx n = fromBand n (0,1,0)

-- TODO: (//), hconcat, vconcat, sizedBlockMx, sizedBlockSMx, blockMx, blockSMx

-- TODO: adding, deleting rows/cols

-- TODO: partitionMx, separateMx

(-#-) :: Num a => TridiagMatrix a -> (VKey,VKey) -> a
m -#- (r,c)
    | dia > err     = error "Out of bound"
    | abs dia > 1   = 0
    | otherwise     = vec (m -!- dia) A.! e
    where   dia = c - r
            e   = max r c - abs dia
            err = dim m - 1

diag :: TridiagMatrix a -> VKey -> Vector a
diag (TDM (_,b,_))  0     = VV b
diag (TDM (a,_,_)) (-1)   = VV a
diag (TDM (_,_,c))  1     = VV c
diag _ _ = undefined

(-!-) :: TridiagMatrix a -> VKey -> Vector a
m -!- k = diag m k

row :: Num a => TridiagMatrix a -> VKey -> Vector a
row m i = vecList $ map (\j -> m -#- (i,j)) [1..n]
    where   n = dim m

col :: Num a => TridiagMatrix a -> VKey -> Vector a
col m j = vecList $ map (\i -> m -#- (i,j)) [1..n]
    where   n = dim m

-- TODO: update, erase

-- TODO: rows, cols, diags

diagonalMx :: (Num a, Eq a) => [a] -> TridiagMatrix a
diagonalMx l = TDM (d0,listArray (1,n) l,d0)
    where   n   = L.length l
            d0  = listArray (1,n-1) $ repeat 0

mainDiag :: TridiagMatrix a -> Vector a
mainDiag = (-!- 0)

fromDiags :: Num a 
    => (Vector a,Vector a,Vector a) -> TridiagMatrix a
fromDiags m@(_,bv,_) = TDM
        (listArray (1,n-1) al
        ,listArray (1,n)   bl
        ,listArray (1,n-1) cl)
    where   n'      = vecLength bv
            n       = max 3 n'
            (al,bl,cl)  = mapTDMx fillVec m

fromBand :: Num a => VKey -> (a,a,a) -> TridiagMatrix a
fromBand n (a,b,c) = TDM
        (listArray (1,n-1) $ repeat a
        ,listArray (1,n)   $ repeat b
        ,listArray (1,n-1) $ repeat c)

fillMx :: Num a => TridiagMatrix a -> [[a]]
fillMx m = map (fillVec . row m) [1..n]
    where   n   = dim m

trans :: TridiagMatrix a -> TridiagMatrix a
trans (TDM (a,b,c)) = TDM (c,b,a)

-- TODO: mulVM

mulMV :: Num a => TridiagMatrix a -> Vector a -> Vector a
m `mulMV` v
    | dim m /= vecLength v = error $
            "mulMV: dimension error, dim m = " ++ show (dim m) ++ ", dim v = "
            ++ show (vecLength v)
    | otherwise = vecList l
    where   entry' i    = sum $ L.zipWith (*) ms vs
                where   ms  = map (\j -> m -#- (i,j)) [a..b]
                        vs  = map (\j -> v Vector.! j) [a..b]
                        a   = max 1 (i - 1)
                        b   = min (dim m) (i + 1)
            l           = L.map entry' [1..(dim m)]

(-*) :: Num a => TridiagMatrix a -> Vector a -> Vector a
(-*) = mulMV

mulSM :: Num a => a -> TridiagMatrix a -> TridiagMatrix a
s `mulSM` m = fmap (*s) m

mulMS :: Num a => TridiagMatrix a -> a -> TridiagMatrix a
m `mulMS` s = s `mulSM` m

(.**) :: Num a => a -> TridiagMatrix a -> TridiagMatrix a
(.**) = mulSM

(**.):: Num a => TridiagMatrix a -> a -> TridiagMatrix a
(**.) = mulMS

mul :: Num a => TridiagMatrix a -> TridiagMatrix a -> TridiagMatrix a
m1 `mul` m2
    | n /= dim m2   = error "mul: dimension error"
    | otherwise = TDM (g,h,j)
    where   aa      = vec $ m1 -!- (-1)
            ba      = vec $ m1 -!- 0
            ca      = vec $ m1 -!- 1
            da      = vec $ m2 -!- (-1)
            ea      = vec $ m2 -!- 0
            fa      = vec $ m2 -!- 1
            n       = dim m1
            mkG i   = aa A.! i * ea A.! i + ba A.! (i+1) * da A.! i
            mkH i
                | i == 1    = ba A.! i * ea A.! i + ca A.! i * da A.! i
                | i == n    = aa A.! (i-1) * fa A.! (i-1) + ba A.! i * ea A.! i
                | otherwise =
                    aa A.! (i-1) * fa A.! (i-1) + ba A.! i * ea A.! i
                    + ca A.! i * da A.! i
            mkJ i   = ba A.! i * fa A.! i + ca A.! i * ea A.! (i+1)
            g       = listArray (1,n-1) $ map mkG [1..(n-1)]
            h       = listArray (1,n) $ map mkH [1..n]
            j       = listArray (1,n-1) $ map mkJ [1..(n-1)]

solve :: (Num a,Fractional a) => TridiagMatrix a -> Vector a -> Vector a
solve m b = vecList x
    where   n   = dim m
            aa  = vec $ m -!- (-1)
            ba  = vec $ m -!- 0
            ca  = vec $ m -!- 1
            da  = vec b
            create_cd i
                | i == 0    = []
                | otherwise = (c,d) : cd'
                where   cd'     = create_cd (i-1)
                        (c',d') = if L.null cd' then (0,0) else L.head cd'
                        ai      = if i==1 then 0 else aa A.! (i-1)
                        bi      = ba A.! i
                        ci      = if i==n then 0 else ca A.! i
                        di      = da A.! i
                        c       = ci/(bi-c'*ai)
                        d       = (di-d'*ai)/(bi-c'*ai)
            cd  = reverse $ create_cd n
            create_x []         = []
            create_x ((c,d):ll) = (d - c * x') : xs
                where   xs = create_x ll
                        x' = if L.null xs then 0 else L.head xs
            x   = create_x cd

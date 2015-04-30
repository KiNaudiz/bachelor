module CNBase
where

import CNTypes
import Tridiag
import Vector

hbar :: (Fractional a) => a
hbar = 0.6582119 -- µeV ns

diffMtx :: RealFloat a => VKey -> Operator a
diffMtx = flip fromBand (1,-2,1)

waveEntries :: (RealFrac a) => Interval a -> a -> Int
waveEntries (x0,xe) dx = ceiling $ (xe-x0)/dx + 1

takeSteps2D :: Int -> Waveset2D a -> Waveset2D a
takeSteps2D i (Waveset2D ws dr dt r0) = Waveset2D (take i ws) dr dt r0

takeTil2D :: RealFrac a => a -> Waveset2D a -> Waveset2D a
takeTil2D a wset = takeSteps2D i wset
    where i = ceiling $ a/wset2DDt wset + 1

takeSteps :: Int -> Waveset a -> Waveset a
takeSteps i (Waveset ws dx dt x0) = Waveset (take i ws) dx dt x0

takeTil :: RealFrac a => a -> Waveset a -> Waveset a
takeTil a wset = takeSteps i wset
    where i = ceiling $ a/wsetDt wset + 1

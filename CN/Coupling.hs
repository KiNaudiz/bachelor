module Coupling
where

import Data.Complex
import CNTypes

-- karth coupling
couplKarth :: (RealFloat a) => a -> Wavepoint a -> a
couplKarth _ phi = magnitude phi ** 2

-- spherical coupling
couplSphere :: (RealFloat a) => a -> Wavepoint a -> a
couplSphere r phi
    | r == 0    = error "couplSphere: singularity"
    | otherwise = magnitude phi ** 2 / r**2


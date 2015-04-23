module Coupling
where

import Data.Complex
import CNTypes

-- karth coupling
couplKarth :: (RealFloat a) => Coupling a
couplKarth _ phi = magnitude phi ** 2

-- spherical coupling
couplSphere :: (RealFloat a) => Coupling a
couplSphere r phi
    | r == 0    = error "couplSphere: singularity"
    | otherwise = magnitude phi ** 2 / r**2

-- karth coupling
coupl2DKarth :: (RealFloat a) => Coupling2D a
coupl2DKarth _ = couplKarth 0

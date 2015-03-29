module CN
where

import Data.Complex
import SymMatrix
import Vector

type Interval a = (a,a)
type Potential a = ( a -> a )
type Wavepoint a = Complex a
type Wave a = [Wavepoint a]

type Operator a = BandMatrix (Complex a)

data System a =
    System
    {
        sysInterval :: Interval a
      , sysMass :: a -- in 0.351764 * mass of an electron
      , sysPotential :: Potential a -- in µeV
      , sysCoupling :: a -- in µeV µm³
    }

data Waveset a =
    Waveset
    {
        wsetWaves :: [Wave a]
      , wsetDx :: a
      , wsetDt :: a
      , wsetX0 :: a
    }

potentialMtx :: (RealFloat a) => System a -> a -> Operator a
potentialMtx sys dx =
        diagonalMx diag'
    where   diag'   = map (\x -> pot x :+ 0) xs
            xs      = takeWhile (<= xe) [x0 + dx * fromInteger h | h <- [0..]]
            (x0,xe) = sysInterval sys
            pot     = sysPotential sys

couplingMtx :: (RealFloat a)
    => System a -> ( a -> Wavepoint a -> a ) -> Wave a -> a -> Operator a
couplingMtx sys coupl wave dx =
        diagonalMx diag'
    where   diag'       = map (\(x,psi) -> couplConst * coupl x psi :+ 0) xpsi
            xpsi        = zip xs wave
            xs          = takeWhile (<= xe)
                [x0 + dx * fromInteger h | h <- [0..]]
            (x0,xe)     = sysInterval sys
            couplConst  = sysCoupling sys

-- karth coupling
couplKarth :: (RealFloat a) => a -> Wavepoint a -> a
couplKarth _ phi = magnitude phi ** 2

-- spherical coupling
couplSphere :: (RealFloat a) => a -> Wavepoint a -> a
couplSphere r phi = magnitude phi ** 2 / r**2

diffMtx :: RealFloat a => BKey -> Operator a
diffMtx n = fromBand n 1 [-2,1]

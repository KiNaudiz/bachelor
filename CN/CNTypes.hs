module CNTypes
where

import Data.Complex
import Vector
import Tridiag

type Interval a = (a,a)
type Potential a = ( a -> a )
type Wavepoint a = Complex a
type Wave a = Vector (Wavepoint a)

type Operator a = TridiagMatrix (Complex a)

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

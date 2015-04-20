module CNTypes
where

import Data.Complex
import Vector
import Tridiag
import ValueMatrix

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

type Interval2D a = ((a,a),(a,a))
type Potential2D a = ( (a,a) -> a )
type Wave2D a = ValueMatrix (Wavepoint a)

data System2D a =
    System2D
    {
        sys2DInterval :: Interval2D a
      , sys2DMass :: a
      , sys2DPotential :: Potential2D a
      , sys2DCoupling :: a
    }

data Waveset2D a =
    Waveset2D
    {
        wset2DWaves :: [Wave2D a]
      , wset2DDr :: (a,a)
      , wset2DDt :: a
      , wset2DR0 :: (a,a)
    }

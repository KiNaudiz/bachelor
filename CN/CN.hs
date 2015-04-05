module CN
where

import Data.Complex
import Tridiag
import Vector

type Interval a = (a,a)
type Potential a = ( a -> a )
type Wavepoint a = Complex a
type Wave a = BandVector (Wavepoint a)

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

hbar :: (Fractional a) => a
hbar = 0.6582119 -- µeV ns

potentialMtx :: (RealFloat a) => System a -> a -> Operator a
potentialMtx sys dx =
        diagonalMx diag'
    where   diag'   = map (\x -> pot x :+ 0) xs
            n       = waveEntries (x0,xe) dx
            xs      = take n [x0 + dx * fromInteger h | h <- [0..]]
            (x0,xe) = sysInterval sys
            pot     = sysPotential sys

couplingMtx :: (RealFloat a)
    => System a -> ( a -> Wavepoint a -> a ) -> Wave a -> a -> Operator a
couplingMtx sys coupl wave dx =
        diagonalMx diag'
    where   diag'       = map (\(x,psi) -> couplConst * coupl x psi :+ 0) xpsi
            n           = waveEntries (x0,xe) dx
            xpsi        = zip xs $ fillVec wave
            xs          = take n
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
diffMtx n = fromBand n (1,-2,1)

renderWave :: (RealFloat a) => (a -> Wavepoint a) -> Interval a -> a -> Wave a
renderWave wave0 int@(x0,_) dx =
        bandList $ map wave0 xs
    where   n   = waveEntries int dx
            xs  = [x0 + fromIntegral m*dx | m <- [0..n-1]]

waveEntries :: (RealFrac a) => Interval a -> a -> Int
waveEntries (x0,xe) dx = ceiling $ (xe-x0)/dx + 1

-- tssp' :: (RealFloat a)
--     => System a -> Wave a -> ( a -> Wavepoint a -> a ) -> a -> a -> Waveset a
-- tssp' system wave0 coupl dx dt =
--         Waveset waves dx dt x0
--     where
--         waves       = iterate timestep wave0
--         i           = 0:+1
--         hbar_c      = hbar :+ 0
--         m_c         = sysMass system :+ 0
--         dx_c        = dx :+ 0
--         dt_c        = dt :+ 0
--         int@(x0,_)  = sysInterval system
--         pot         = potentialMtx system dx
--         n           = waveEntries int dx
--         dmtx        = diffMtx n
--         mmtx        = idMx n  + ((1/12) .** dmtx)
--         timestep w' = solve lmx b
--             where   b       = (mmtx -* w') - (lmx' -* w')
--                     lmx'    = (i*dt_c/2)
--                             .** (mmtx `mul` pot' - ((hbar_c**2/(2*dx_c**2*m_c)) .** dmtx))
--                     lmx     = mmtx + lmx'
--                     pot'    = pot + couplingMtx system coupl w' dx

effPot :: (RealFloat a)
    => System a -> ( a -> Wavepoint a -> a ) -> a -> Wavepoint a -> a
effPot system coupl x phi = sysPotential system x +
    sysCoupling system * coupl x phi

tssp' :: (RealFloat a)
    => System a -> Wave a -> ( a -> Wavepoint a -> a ) -> a -> a -> Waveset a
tssp' system wave0 coupl dx dt =
        Waveset waves dx dt x0
    where
        waves       = iterate timestep wave0
        i           = 0:+1
        hbar_c      = hbar :+ 0
        m_c         = sysMass system :+ 0
        dx_c        = dx :+ 0
        dt_c        = dt :+ 0
        int@(x0,_)  = sysInterval system
        n           = waveEntries int dx
        dmtx        = diffMtx n
        mmtx        = idMx n + ((1/12) .** dmtx)
        timestep    = applHPot . applKin . applHPot
        -- timestep    = applKin
        applHPot w' = bandList w
            where   wl          = fillVec w'
                    w           = step wl x0
                    step [] _           = []
                    step (wh:wr) x      = applyHalfPot x wh dt pot : step wr (x+dx)
        pot         = effPot system coupl
        -- pot x _     = x**2
        applKin w'  = solve lmx b
            where   b       = (mmtx -* w') - (lmx' -* w')
                    lmx'    = (-i*dt_c*hbar_c**2/(4*dx_c**2*m_c)) .** dmtx
                    lmx     = mmtx + lmx'

applyHalfPot :: RealFloat a
    => a -> Wavepoint a -> a -> (a -> Wavepoint a -> a) -> Wavepoint a
applyHalfPot x phi dt pot =
        exp(-i * dt_c * v / ( 2 * hbar_c )) * phi
    where   v   = pot x phi :+ 0
            i   = 0:+1
            hbar_c  = hbar :+ 0
            dt_c    = dt :+ 0

tssp :: (RealFloat a) => System a -> (a -> Wavepoint a) -> a -> a -> Waveset a
tssp system wave0 dx dt =
        tssp' system wave0' couplKarth dx dt
    where   int     = sysInterval system
            wave0'  = renderWave wave0 int dx

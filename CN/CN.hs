module CN
where

import Data.Complex
import Tridiag
import Vector
import CNTypes
import CNBase
import Coupling

renderWave :: (RealFloat a) => (a -> Wavepoint a) -> Interval a -> a -> Wave a
renderWave wave0 int@(x0,_) dx =
        vecList $ map wave0 xs
    where   n   = waveEntries int dx
            xs  = [x0 + fromIntegral m*dx | m <- [0..n-1]]

effPot :: (RealFloat a)
    => System a -> ( a -> Wavepoint a -> a ) -> a -> Wavepoint a -> a
effPot system coupl x phi = sysPotential system x
    + sysCoupling system * coupl x phi

-- with predictor
cn' :: (RealFloat a)
    => System a -> Wave a -> ( a -> Wavepoint a -> a ) -> a -> a -> Waveset a
cn' system wave0 coupl dx dt =
        Waveset waves dx dt x0
    where
        waves           = iterate timestep wave0
        i               = 0:+1
        hbar_c          = hbar :+ 0
        m_c             = sysMass system :+ 0
        dx_c            = dx :+ 0
        dt_c            = dt :+ 0
        int@(x0,_)      = sysInterval system
        n               = waveEntries int dx
        dmtx            = diffMtx n
        mmtx            = idMx n + ((1/12) .** dmtx)
        timestep' p'    = applHPot p' . applKin . applHPot p'

        -- predictor CN avg
        timestep  w     = timestep' p' w
            where   p'  = 0.5 .* (timestep' w w + w)

        -- -- predictor CN
        -- timestep  w     = timestep' p' w
        --     where   p'  = timestep' w w

        -- -- no predictor
        -- timestep  w     = timestep' w w

        applHPot p' w'  = vecList (Prelude.zipWith (*) potl wl)
            where   wl          = fillVec w'
                    pl          = fillVec p'
                    potl        = step pl x0
                    step [] _           = []
                    step [e] _          = [e]
                    step (wh:wr) x      =
                        applyHalfPot x wh dt pot : step wr (x+dx)
        pot         = effPot system coupl
        -- pot x _     = x**2
        applKin w'  = solve lmx b
            where   b       = (mmtx -* w') + (lmx' -* w')
                    lmx'    = (i*dt_c*hbar_c/(4*m_c*dx_c**2)) .** dmtx
                    lmx     = mmtx - lmx'

applyHalfPot :: RealFloat a
    => a -> Wavepoint a -> a -> (a -> Wavepoint a -> a) -> Wavepoint a
applyHalfPot x phi dt pot =
        exp(-i * dt_c * v / ( 2 * hbar_c ))
    where   v   = pot x phi :+ 0
            i   = 0:+1
            hbar_c  = hbar :+ 0
            dt_c    = dt :+ 0

cn :: (RealFloat a) => System a -> (a -> Wavepoint a) -> a -> a -> Waveset a
cn system wave0 dx dt =
        cn' system wave0' couplKarth dx dt
    where   int     = sysInterval system
            wave0'  = renderWave wave0 int dx

-- helper function
backtransSphere :: (RealFloat a) => Waveset a -> Waveset a
backtransSphere wset =
        wset'
    where   wset'   = wset { wsetWaves = waves' }
            waves   = wsetWaves wset
            x0      = wsetX0 wset
            dx      = wsetDx wset
            waves'  = map (vecList . (`trans'` x0) . fillVec) waves
            trans' [] _         = []
            trans' (_:wl) 0     = 0 : trans' wl dx
            trans' (wh:wl) x    = wh/(x:+0) : trans' wl (x+dx)

cnSphere :: (RealFloat a)
    => System a -> (a -> Wavepoint a) -> a -> a -> Waveset a
cnSphere system wave0 dx dt =
        waves0'
    where   int     = sysInterval system
            p0 x    = wave0 x * (x:+0)
            p0'     = renderWave p0 int dx
            res'    = cn' system p0' couplSphere dx dt
            waves0' = backtransSphere res'

energy :: (RealFloat a) => System a -> a -> Wave a -> a
energy sys dx wave = e * dx
    where   pot             = effPot sys couplKarth
            (x0,_)          = sysInterval sys
            m               = sysMass sys
            n               = vecLength wave
            kin             = hbar**2/(2*m)
                            * magnitude (sum $ fillVec $ diffMtx n -* wave)
            pot' [] _       = 0
            pot' (lh:ll) x  = pot x lh + pot' ll (x+dx)
            e               = pot' (fillVec wave) x0 + kin

energySphere :: (RealFloat a) => System a -> a -> Wave a -> a
energySphere sys dx wave = 4*pi*e*dx
    where   pot             = effPot sys couplKarth
            (x0,_)          = sysInterval sys
            m               = sysMass sys
            n               = vecLength wave
            kin             = map ((*(hbar**2/(2*m))) . magnitude)
                                (fillVec $ diffMtx n -* wave)
            pot' [] _       = []
            pot' (lh:ll) x  = pot x lh : pot' ll (x+dx)
            kinpot          = Prelude.zipWith (+) (pot' (fillVec wave) x0) kin
            tmsx _ []       = 0
            tmsx x (lh:ll)  = x**2 * lh + tmsx (x+dx) ll
            e               = tmsx x0 kinpot

density :: (RealFloat a) => a -> Wave a -> a
density dx = (*dx) . sum . map ((**2) . magnitude) . fillVec

densitySphere :: (RealFloat a) => a -> a -> Wave a -> a
densitySphere x0 dx =
        (*(dx*4*pi)) . tmsx x0 . map ((**2) . magnitude) . fillVec
    where   tmsx _ []       = 0
            tmsx x (lh:ll)  = x**2 * lh + tmsx (x+dx) ll

--------------------------------------------------------------------------------
-- | 
-- Module      : TSSP (1D)
-- Note        : one-dimensional solution for NLSE
-- 
--------------------------------------------------------------------------------

--   mass: 0.351764 mass of an electron
--   energy: µeV
--   distance: µm
--   time:  ns

module TSSP
    ( System (..)
    , Interval
    , Potential
    , Wavepoint
    , Wave
    , Waveset (..)
    , hbar
    , tssp'
    , tssp
    , tsspSphere
    , renderWave
    )
where

import Data.Complex

--------------------------------------------------------------------------------
--  data structures

type Interval a = (a,a)
type Potential a = ( a -> a )
type Wavepoint a = Complex a
type Wave a = [Wavepoint a]

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

--------------------------------------------------------------------------------
--  helper functions

-- effective potential = extern potential + coupling
effPot :: (RealFloat a) 
    => System a -> ( a -> Wavepoint a -> a ) -> a -> Wavepoint a -> a
effPot system coupl x phi = sysPotential system x +
    sysCoupling system * coupl x phi

-- karth coupling
couplKarth :: (RealFloat a) => a -> Wavepoint a -> a
couplKarth _ phi = magnitude phi ** 2

-- spherical coupling
couplSphere :: (RealFloat a) => a -> Wavepoint a -> a
couplSphere r phi = magnitude phi ** 2 / r**2

-- hbar constant
hbar :: (Fractional a) => a
hbar = 0.6582119 -- µeV ns

-- tssp using a list as starting condition
tssp' :: (RealFloat a)
    => System a -> Wave a -> ( a -> Wavepoint a -> a ) -> a -> a -> Waveset a
tssp' system wave0 coupl dx dt =
        Waveset waves dx dt x0
    where   int@(x0,_)  = sysInterval system
            len         = length wave0 - 1
            pot         = effPot system coupl
            waves       = iterate timestep wave0
            m           = sysMass system

            timestep w  = wave4 
                where
                    wave1@(wh1:wl1)     = step w x0
                        where   step [] _       = []
                                step (wh:wl) x  =
                                    applyHalfPot x wh dt pot : step wl (x+dx)
                    wave2@(wh2:wl2)     = wh1 : step wl1 1
                        where   step [] _           = undefined
                                step (wh':[]) _     = [wh']
                                step (_:wl') l    =
                                    normalize wave1 len l : step wl' (l+1)
                    wave3  = wh2 : step wl2 1
                        where   step [] _           = undefined
                                step (wh':[]) _     = [wh']
                                step (_:wl') j    =
                                    applyKin wave2 int dt m len j
                                    : step wl' (j+1)
                    wave4               = step wave3 x0
                        where   step [] _       = []
                                step (wh:wl) x  =
                                    applyHalfPot x wh dt pot
                                    : step wl (x+dx)

-- halfstep for effective potential
applyHalfPot :: RealFloat a
    => a -> Wavepoint a -> a -> (a -> Wavepoint a -> a) -> Wavepoint a
applyHalfPot x phi dt pot =
        exp(-i * dt_c * v / ( 2 * hbar_c )) * phi
    where   v   = pot x phi :+ 0
            i   = 0:+1
            hbar_c  = hbar :+ 0
            dt_c    = dt :+ 0

-- normalize wave
normalize :: (RealFloat a) => Wave a -> Int -> Int -> Wavepoint a
normalize [] _ _        = undefined
normalize (_:wl) len l  =
        2/fromIntegral len * summands wl 1
    where   summands  [] _          = undefined
            summands (_:[]) _       = 0
            summands (wh':wl') j    =
                wh'*sin(fromIntegral(l*j)*pi/fromIntegral len) +
                summands wl' (j+1)

-- step for kinetic energy
applyKin :: (RealFloat a)
    => Wave a -> Interval a -> a -> a -> Int -> Int -> Wavepoint a
applyKin [] _ _ _ _ _       = undefined
applyKin (_:wl) int dt m len j  =
        summands wl 1
    where   summands  [] _          = undefined
            summands (_:[]) _       = 0
            summands (wh':wl') l    =
                    exp(-i*dt_c*hbar_c*mu_l2/(2*m_c)) *
                    wh'*sin(pi*fromIntegral(l*j)/fromIntegral len) +
                    summands wl' (l+1)
                where   mu_l    = mu int l
                        mu_l2   = mu_l**2 :+ 0
            m_c     = m :+ 0
            dt_c    = dt :+ 0
            i       = 0:+1
            hbar_c  = hbar :+ 0

-- helper function
mu :: Floating a => Interval a -> Int -> a
mu (x0,xe) l    = pi*fromIntegral l/(xe-x0)

-- helper function
backtransSphere :: (RealFloat a) => Waveset a -> Waveset a
backtransSphere wset =
        wset'
    where   wset'   = wset { wsetWaves = waves' }
            waves   = wsetWaves wset
            x0      = wsetX0 wset
            dx      = wsetDx wset
            waves'  = map (`trans` x0) waves
            trans [] _      = []
            trans (_:wl) 0  = 0 : trans wl dx
            trans (wh:wl) x = wh/(x:+0) : trans wl (x+dx)

-- render a wave function to a list
renderWave :: (RealFloat a) => (a -> Wavepoint a) -> Interval a -> a -> Wave a
renderWave wave0 (x0,xe) dx =
        map wave0 xs
    where   n   = (ceiling $ (xe-x0)/dx) :: Integer
            xs  = [x0 + fromInteger m*dx | m <- [0..n-1]]

--------------------------------------------------------------------------------
--  Function solving NLSE

tssp :: (RealFloat a) => System a -> (a -> Wavepoint a) -> a -> a -> Waveset a
tssp system wave0 dx dt =
        tssp' system wave0' couplKarth dx dt
    where   int     = sysInterval system
            wave0'  = renderWave wave0 int dx

tsspSphere :: (RealFloat a) => System a -> (a -> Wavepoint a) -> a -> a -> Waveset a
tsspSphere system wave0 dx dt =
        waves0'
    where   int     = sysInterval system
            p0 x    = wave0 x * (x:+0)
            p0'     = renderWave p0 int dx
            res'    = tssp' system p0' couplSphere dx dt
            waves0' = backtransSphere res'

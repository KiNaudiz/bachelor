{-# LANGUAGE AllowAmbiguousTypes #-}--
{-# OPTIONS -Wall #-}
-- TSSP (time-splitting sine pseudospectral) method
-- units:
--   mass: 0.351764 mass of an electron
--   energy: µeV
--   distance: µm
--   time:  ns

import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Tuple

type Potential a    = (a -> Complex a -> a)
type Interval a     = (a,a)
type Wavepoint a    = Complex a
type Wave a         = [Wavepoint a]

mu :: (Floating a) => Int -> Interval a -> a
mu l (x0,xe) = pi*fromIntegral l/(xe-x0)

step1 :: (RealFloat a)
    => Wavepoint a -> Potential a -> a -> a -> a -> Wavepoint a
step1 p u x dt e = exp(-i*dt'*u'/(2*e')) * p
    where   i   = 0:+1
            dt' = dt :+ 0
            e'  = e :+ 0
            u'  = u x p :+ 0

step2 :: (RealFloat a)
    => Wave a -> Int -> Int -> Wavepoint a
step2 w l j =
        2/fromIntegral l * summands w 1
    where   summands [] _ = 0
            summands (wh:wl) j' =
                wh*sin(fromIntegral(j*j')*pi/fromIntegral l) + summands wl (j'+1)

step3 :: (RealFloat a) =>
    Wave a -> Interval a -> a -> a -> a -> Int -> Int -> Wavepoint a
step3 w (x0,xe) dt e m l j =
        summands w 1
    where   summands [] _   = 0
            summands (wh':wl') l' = exp(-i*dt'*e'*mu'**2/(2*m')) * wh' *
                    sin(fromIntegral (l'*j)*pi/fromIntegral l) +
                    summands wl' (l'+1)
                where   i   = 0:+1
                        e'  = e:+0
                        m'  = m:+0
                        mu' = mu l' (x0,xe) :+0
                        dt' = dt:+0

step4 :: (RealFloat a) =>
    Wave a -> Wavepoint a -> Interval a -> Potential a -> a -> a -> a -> a -> a -> Int -> Int -> Wavepoint a
step4 [] _ _ _ _ _ _ _ _ _ _  = undefined
step4 (_:wl) p int@(x0,_) u x dx dt e m l j =
        exp(-i*dt'*u'/(2*e')) * step3 w'' int dt e m l j
    where   i   = 0:+1
            e'  = e:+0
            dt' = dt:+0
            u'  = u x p :+ 0
            -- w'  = map (\p' -> step2 p' u x dt e l j) w -- FIXME: x = x(l)
            w'  = runstep1 wl (x0+dx)
            runstep1 [] _       = undefined
            runstep1 (_:[]) _   = []
            runstep1 (wh':wl') x' =
                step1 wh' u x' dt e : runstep1 wl' (x'+dx)
            w'' = runstep2 wl (x0+dx)
            runstep2 [] _       = undefined
            runstep2 (_:[]) _   = []
            runstep2 (_:wl') x' =
                step2 w' l j : runstep2 wl' (x'+dx)

timestep :: (RealFloat a,Num a) 
    => Wave a -> Interval a -> a -> a -> Potential a -> a -> a -> Int -> Wave a
timestep [] _ _ _ _ _ _ _ = undefined
timestep w@(wh:wl) int@(x0,_) dx dt u e m l = wh : locs wl (x0+dx) 1
    where   locs [] _ _         = undefined
            locs (wh':[]) _ _   = [wh']
            locs (wh':wl') x j  =
                step4 w wh' int u x dx dt e m l j : locs wl' (x+dx) (j+1)

initFromList :: (RealFloat a) =>
    Wave a -> Interval a -> a -> a -> Potential a -> a -> a -> [Wave a]
initFromList w int dx dt u e m =
        iterate (\w' -> timestep w' int dx dt u e m l) w
    where   l = length w - 1

initFromFun :: (RealFloat a, Num a) =>
    ( a -> Complex a ) -> Interval a -> a -> a -> Potential a -> a -> a -> [Wave a]
initFromFun f int@(x0,xe) dx dt u e m = initFromList w int dx dt u e m
    where   w = next x0
            next x
                | x <= xe   = f x : next (x+dx)
                | otherwise = []

main :: IO ()
main = do
        let a       = 0.1 -- µeV µm^2
            -- g       = 1
            u x phi = a * x**2 -- + g * magnitude phi **2
            e       = 0.6582119 -- µeV ns (= hbar)
            dt      = 0.0001 :: Double -- ns
            dx      = 0.025:: Double -- µm
            x0      = -15.0
            m       = 7.4 :: Double -- 2.6 m_e
            -- phi0    = [0,0,0,1,1,0,0,0] :: [Complex Double]
            mu'     = 1 :: Double
            sigma   = 0.01 :: Double
            interval= (x0,3.0)
            phi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu')**2/(2*sigma**2)) :+ 0
            phiT    = take 100 $ initFromFun phi0 interval dx dt u e m :: [Wave Double]
            -- phi0    = [0,1,2,3,0]
            -- phiT    = take 10 $ initFromList phi0 interval dx dt u e :: [Wave Double]
        -- putStr $ unlines $ map show phiT
        plotManyComplex phiT x0 dt dx
        return ()

plotComplex :: (Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => [Attribute] -> [Complex a] -> a -> a -> IO ()
plotComplex a l x0 dx = do
        let real = addLoc x0 $ map realPart l
            imag = addLoc x0 $ map imagPart l
            density = addLoc x0 $ map ((**2) . magnitude) l
        plotLists a [real,imag,density];
        -- plotLists a [density];
    where   addLoc _ []        = []
            addLoc x (lh:ll)   = (x,lh) : addLoc (x+dx) ll

plotManyComplex :: (Show a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => [[Complex a]] -> a -> a -> a -> IO ()
plotManyComplex l x0 dt dx = p l 0
    where
        p [] _ = return ()
        p (lh:ll) t = do
            -- plotComplex [PNG ("output/plot"++show t),Title ("t="++show t++"ns"),XLabel="x/us",YLabel="phi"] lh dx
            plotComplex [PNG ("output/plot"++show t),Title ("t="++show t++"ns")] lh x0 dx
            p ll (t+dt)

-- TSSP (time-splitting sine pseudospectral) method
-- units:
--   mass: m_e (mass of an electron)
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

mu :: (Floating a,Integral b) => b -> Interval a -> a
mu l (x0,xe) = pi*fromIntegral l/(xe-x0)

step1 :: (RealFloat a)
    => Wavepoint a -> Potential a -> a -> a -> a -> Wavepoint a
step1 p u x dt e = exp(-i*dt'*u'/(2*e')) * p
    where   i   = 0:+1
            dt' = dt :+ 0
            e'  = e :+ 0
            u'  = u x p :+ 0

step2 :: (Integral b,RealFloat a)
    => Wavepoint a -> Potential a -> a -> a -> a -> b -> b -> Wavepoint a
step2 p u x dt e l j =
        2/fromIntegral j * summands 1
    where   p' = step1 p u x dt e
            summands j'
                | j' == j   = 0
                | otherwise =
                    p'*sin(fromIntegral(l*j')*pi/fromIntegral j) + summands j'+1

step3 :: (Integral b,RealFloat a) =>
    Wave a -> Interval a -> a -> a -> b -> b -> Wavepoint a
step3 w (x0,xe) dt e l j =
        summands wl 1
    where   (_:wl)             = w
            summands [] _       = undefined
            summands (_:[]) _   = 0
            summands (wh':wl') l' = exp(-i*dt'*e'*mu') * wh' *
                    sin(fromIntegral (l'*j)*pi/fromIntegral l) +
                    summands wl' (l'+1)
                where   i   = 0:+1
                        e'  = e:+0
                        mu' = mu l' (x0,xe) :+0
                        dt' = dt:+0

step4 :: (Integral b,RealFloat a) =>
    Wave a -> Wavepoint a -> Interval a -> Potential a -> a -> a -> a -> b -> b -> Wavepoint a
step4 w p int u x dt e l j =
        exp(-i*dt'*u'/(2*e')) * step3 w int x dt e l j
    where   i   = 0:+1
            e'  = e:+0
            dt' = dt:+0
            u'  = u x p :+ 0
            w'  = map (\p' -> step2 p' u x dt e l j) w

timestep :: (Integral b,RealFloat a,Num a) 
    => Wave a -> Interval a -> a -> a -> Potential a-> a -> b -> Wave a
timestep [] _ _ _ _ _ _ = undefined
timestep w@(wh:wl) int@(x0,_) dx dt u e l = wh : locs wl x0 0
    where   locs [] _ _         = undefined
            locs (wh':[]) _ _   = []
            locs (wh':wl') x j  =
                step4 w wh' int u x dt e l j : locs wl' (x+dx) (j+1)

initFromList :: (RealFloat a,Integral b) =>
    Wave a -> Interval a -> a -> a -> Potential a -> a -> [Wave a]
initFromList w int dx dt u e =
        iterate (\w' -> timestep w int dx dt u e l) w
    where   l = length w

initFromFun :: (RealFloat a, Num a) =>
    ( a -> Complex a ) -> Interval a -> a -> a -> Potential a -> a -> [Wave a]
initFromFun f int@(x0,xe) dx dt u e = initFromList w int dx dt u e
    where   w = next x0
            next x
                | x <= xe   = f x : next (x+dx)
                | otherwise = []

main :: IO ()
main = do
        let a       = 0.1 -- µeV µm^2
            g       = 5*10**(-4) -- µeV µm^3
            u x phi = a * x*x + g * magnitude phi **2
            e       = 0.6582119 -- µeV ns (= hbar)
            dt      = 0.1 :: Double -- ns
            dx      = 0.01 :: Double -- µm
            x0      = -2.0
            -- m       = 2.6 -- m_e
            -- phi0    = [0,0,0,1,1,0,0,0] :: [Complex Double]
            mu      = 1.0 :: Double
            sigma   = 0.1 :: Double
            interval= (x0,2.0)
            phi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0
            phiT    = take 500 $ initFromFun phi0 interval dx dt u e :: [Wave Double]
        putStr $ unlines $ map show phiT
        plotManyComplex phiT x0 dt dx
        return ()

plotComplex :: (Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => [Attribute] -> [Complex a] -> a -> a -> IO ()
plotComplex a l x0 dx = do
        let real = addLoc x0 $ map realPart l
            imag = addLoc x0 $ map imagPart l
        plotLists a [real,imag];
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

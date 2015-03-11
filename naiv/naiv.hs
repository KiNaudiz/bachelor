-- naive solution of
--   ie d(phi)/dt = -e^2/(2m) d^2(phi)/dx^2 + U(x) phi
-- asatz:
--   d(phi)/dt = (phi(t+dt) - phi(t))/dt
--   d^2(phi)/dx^2 = (phi(x-dx)-2 phi(x) + phi(t,x+dx))/dx^2
-- units:
--   mass: m_e (mass of an electron)
--   energy: µeV
--   distance: µm
--   time:  ns

import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Tuple

timestep :: (RealFloat a, Num a) =>
    [Complex a] -> a -> a -> a -> ( Complex a -> Complex a ) -> a -> a -> [Complex a]
timestep [] _ _ _ _ _ _ = []
timestep (lh:ls) x0 dt dx u e m = lh : xstep lh ls x0
    where   --xstep :: (Num a) => Complex a -> [Complex a] -> [Complex a]
            xstep _ [] _            = []
            xstep _ (s1:[]) _       = [s1]
            xstep s0 (s1:s2:ss) x   = calc s0 s1 s2 x dt dx u e m : xstep s1 (s2:ss) (x+dx)
calc :: (RealFloat a, Num a) => Complex a -> Complex a -> Complex a -> a -> a -> a -> ( Complex a -> Complex a) -> a -> a -> Complex a
calc s0 s1 s2 x dt dx u e m =
        (0:+1) * dt' *
        ( e' * ( s0 - 2*s1 + s2 ) /  (2*m'*dx'**2) -
          u x' * s1 / e' )
    where
        e'  = e:+0
        m'  = m:+0
        dt' = dt:+0
        dx' = dx:+0
        x'  = x:+0

initFromList :: (RealFloat a, Num a) =>
    [Complex a] -> a -> a -> a -> ( Complex a -> Complex a ) -> a -> a -> [[Complex a]]
initFromList l x0 dt dx u e m =
        iterate (\l' -> timestep l' x0 dt dx u e m) l

initFromFun :: (RealFloat a, Num a) =>
    ( a -> Complex a ) -> (a,a) -> a -> a -> ( Complex a -> Complex a ) -> a ->
    a -> [[Complex a]]
initFromFun f (x0,xe) dt dx = initFromList l x0 dt dx
    where   l = next x0
            next x
                | x <= xe   = f x : next (x+dx)
                | otherwise = []

main :: IO ()
main = do
        let a       = 0.1:+0 -- µeV µm^2
            g       = 5*10**(-4):+0 -- µeV µm^3 
            u x     = a * x*x + g * (magnitude x :+ 0) * x
            e       = 0.6582119 -- µeV ns (= hbar)
            dt      = 0.1 :: Double -- ns
            dx      = 0.01 :: Double -- µm
            x0      = -2.0
            m       = 2.6 -- m_e
            -- phi0    = [0,0,0,1,1,0,0,0] :: [Complex Double]
            mu      = 1.0 :: Double
            sigma   = 0.1 :: Double
            interval= (x0,2.0)
            phi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0
            phiT    = take 500 $ initFromFun phi0 interval dt dx u e m :: [[Complex Double]]
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

-- plotComplex l dt = do
--         let real    = map (realPart . addTime 0) l
--             imag    = map (imagPart . addTime 0) l
--             lists   = [real,imag]
--         plotLists [] lists
--     where   addTime _ _         = []
--             addTime t (lh:ll)   = (t,lh) : addTime (t+dt) ll


-- toFile :: (Show a) => [[a]] -> String-> IO ()
-- toFile l filename = do
--         

import CN
import Plot
import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Tuple
import Vector
import Tridiag
import Text.Printf
import File
import System.Process

main :: IO ()
-- main = harmOszSphere
-- main = hydrogen
main = do
        -- _ <- harmOszSphere
        _ <- harmOsz
        return ()

harmOszSphere :: IO (Waveset Double)
harmOszSphere = do
        let int'    = (0.0,10)
            sys'    = sys { sysInterval = int' }
            waveT   = takeTil 40 $ tssp sys' psi0 dx dt
            title = "harmpot_sphere/data_norm_coupl_split_dx" ++ printf "%.4f" dx
                    ++ "_dt" ++ printf "%.3f" dt
        _ <- createProcess $ shell $ "mkdir -p output/" ++ title
        plotWaveset waveT (0,5) (-0.1,1.1) $ title ++ "/"
        writeWaveset waveT $ "output/" ++ title ++ ".dat"
        _ <- plotWaveset waveT (0,5) (-0.1,1.1) $ title ++ "/"
        _ <- plotDensity waveT $ title ++ "/"
        return waveT

m,a,g :: Double
m       = 7.4 -- 2.6 m_e
a       = 0.1
-- a = 0 :: Double
g       = 5*10**(-4)
-- g = 0
u :: Potential Double
u x     = a*x*x -- harm
int :: Interval Double
int     = (-10,10)
sys :: System Double
sys     = System int m u g

sigma,mu :: Double
sigma   = 0.5
mu      = 2
psi0 :: Double -> Complex Double
psi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0

dx,dt :: Double
dx      = 0.05
dt      = 0.5

harmOsz :: IO (Waveset Double)
harmOsz = do
        let
            waveT   = takeTil 40 $ tssp sys psi0 dx dt
            title = "harmpot/data_norm_coupl_split_dx" ++ printf "%.4f" dx
                    ++ "_dt" ++ printf "%.3f" dt
        _ <- createProcess $ shell $ "mkdir -p output/" ++ title
        plotWaveset waveT (-5,5) (-0.1,1.1) $ title ++ "/"
        writeWaveset waveT $ "output/" ++ title ++ ".dat"
        _ <- plotEnergy sys waveT $ title ++ "/"
        _ <- plotDensity waveT $ title ++ "/"
        return waveT

plotWaveset :: (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => Waveset a -> Interval Double -> Interval Double -> String -> IO ()
plotWaveset set xr yr fname = do
        let list = map fillVec $ wsetWaves set
            dt'  = wsetDt set
            dx'  = wsetDx set
            x0   = wsetX0 set
            dens = densityList set
        plotManyComplex [XLabel "x/um",YLabel "|psi|^2",XRange xr,YRange yr] fname list x0 dt' dx'
        -- plotManyComplex [XLabel "x/um",YLabel "|psi|^2"] fname list x0 dt dx

plotDensity :: (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => Waveset a -> String -> IO [(a,a)]
plotDensity set fname = do
        let dens = densityList set
        plotList [Key Nothing,PNG ("output/" ++ fname ++ "density.png")
                 ,XLabel "t/ns",YLabel "|psi|^2",Title "|psi(t)|^2"] dens
        return dens

plotEnergy :: (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a)
    => System a -> Waveset a -> String -> IO [(a,a)]
plotEnergy sys' wset fname = do
        let en   = energyList sys' wset
        plotList [Key Nothing,PNG ("output/" ++ fname ++ "energy.png")
                 ,XLabel "t/ns",YLabel "E=T+U",Title "E(t) 1-dim"] en
        return en

energyList :: (RealFloat a)
    => System a -> Waveset a -> [(a,a)]
energyList sys' wset = addPar (wsetDt wset) 0 $
        map (energy sys' (wsetDx wset)) $ wsetWaves wset

densityList :: RealFloat a => Waveset a -> [(a,a)]
densityList wset = addPar (wsetDt wset) 0
        $ map ( (* wsetDx wset) . sum . map ( (**2) . magnitude) . fillVec )
        $ wsetWaves wset

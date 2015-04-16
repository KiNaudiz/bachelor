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
        _ <- harmOszSphere
        -- _ <- harmOsz
        return ()

harmOszSphere :: IO (Waveset Double)
harmOszSphere = do
        let int'    = (dx,mu*pnum*5)
            sys'    = sys { sysInterval = int' }
            waveT   = takeTil 40 $ tsspSphere sys' psi0 dx dt
            psi0Norm    = densitySphere dx dx
                        $ renderWave psi0Form int' dx
            psi0    = (*(sqrt (pnum/psi0Norm) :+0)) . psi0Form
            title = "harmpot_sphere/data_pred_split_mu"
                    ++ printf "%3.1f" mu ++ "_g" ++ printf "%.7f" g
                    ++ "_pnum" ++ printf "%f" pnum
                    ++ "_dx" ++ printf "%.4f" dx
                    ++ "_dt" ++ printf "%.3f" dt
        _ <- createProcess $ shell $ "mkdir -p output/" ++ title
        plotWaveset waveT (0,4) (-2,pnum) $ title ++ "/"
        writeWaveset waveT $ "output/" ++ title ++ ".dat"
        _ <- plotEnergySphere sys' waveT $ title ++ "/"
        _ <- plotDensitySphere waveT $ title ++ "/"
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
sigma   = 0.2
mu      = 2.0

pnum :: Double
pnum    = 10

psi0Form :: Double -> Complex Double
psi0Form x  = (1/sigma) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0

dx,dt :: Double
dx      = 0.05
dt      = 0.5

harmOsz :: IO (Waveset Double)
harmOsz = do
        let
            psi0Norm    = density dx
                        $ renderWave psi0Form int dx
            psi0    = (* ((pnum/psi0Norm) :+0)) . psi0Form
            waveT   = takeTil 40 $ tssp sys psi0 dx dt
            title = "harmpot/data_norm_coupl_split_dx" ++ printf "%.4f" dx
                    ++ "_dt" ++ printf "%.3f" dt
        _ <- createProcess $ shell $ "mkdir -p output/" ++ title
        plotWaveset waveT (-5,5) (-0.1,1.1) $ title ++ "/"
        writeWaveset waveT $ "output/" ++ title ++ ".dat"
        _ <- plotEnergy sys waveT $ title ++ "/"
        _ <- plotDensity waveT $ title ++ "/"
        return waveT

plotWaveset :: (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a,RealFloat a,Num a)
    => Waveset a -> Interval Double -> Interval Double -> String -> IO ()
plotWaveset set xr yr fname = do
        let list = map fillVec $ wsetWaves set
            dt'  = wsetDt set
            dx'  = wsetDx set
            x0   = wsetX0 set
        plotManyComplex [XLabel "x/um",YLabel "|psi|^2",XRange xr,YRange yr]
            fname list x0 dt' dx'
        -- plotManyComplex [XLabel "x/um",YLabel "|psi|^2"] fname list x0 dt dx

plotDensity :: (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a,RealFloat a,Num a)
    => Waveset a -> String -> IO [(a,a)]
plotDensity set fname = do
        let dens = densityList set
        plotList [Key Nothing,PNG ("output/" ++ fname ++ "density.png")
                 ,XLabel "t/ns",YLabel "|psi|^2",Title "|psi(t)|^2"] dens
        return dens

plotDensitySphere ::
    (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a,RealFloat a,Num a)
    => Waveset a -> String -> IO [(a,a)]
plotDensitySphere set fname = do
        let dens = densityListSphere set
        plotList [Key Nothing,PNG ("output/" ++ fname ++ "density.png")
                 ,XLabel "t/ns",YLabel "|psi|^2",Title "|psi(t)|^2"] dens
        return dens

plotEnergy :: (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a)
    => System a -> Waveset a -> String -> IO [(a,a)]
plotEnergy sys' wset fname = do
        let en   = energyList sys' wset
        plotList [Key Nothing,PNG ("output/" ++ fname ++ "energy.png")
                 ,XLabel "t/ns",YLabel "E=T+U/eV",Title "E(t) 1-dim"] en
        return en

plotEnergySphere :: (PrintfArg a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a)
    => System a -> Waveset a -> String -> IO [(a,a)]
plotEnergySphere sys' wset fname = do
        let en   = energyListSphere sys' wset
        plotList [Key Nothing,PNG ("output/" ++ fname ++ "energy.png")
                 ,XLabel "t/ns",YLabel "E=T+U/eV",Title "E(t)"] en
        return en

energyList :: (RealFloat a)
    => System a -> Waveset a -> [(a,a)]
energyList sys' wset = addPar (wsetDt wset) 0 $
        map (energy sys' (wsetDx wset)) $ wsetWaves wset

energyListSphere :: (RealFloat a)
    => System a -> Waveset a -> [(a,a)]
energyListSphere sys' wset = addPar (wsetDt wset) 0 $
        map (energySphere sys' (wsetDx wset)) $ wsetWaves wset

densityList :: RealFloat a => Waveset a -> [(a,a)]
densityList wset = addPar (wsetDt wset) 0
        $ map (density (wsetDx wset))
        $ wsetWaves wset

densityListSphere :: RealFloat a => Waveset a -> [(a,a)]
densityListSphere wset = addPar (wsetDt wset) 0
        $ map (densitySphere (wsetX0 wset) (wsetDx wset))
        $ wsetWaves wset


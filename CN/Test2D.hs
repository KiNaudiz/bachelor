module Test2D
where

import CN2D
import CNTypes
import CNBase
import Data.Complex
import Plot2D

harmOsz2D :: IO (Waveset2D Double)
harmOsz2D = do
        let
--             psi0Norm    = density dx
--                         $ renderWave psi0Form int dx
--             psi0    = (* ((pnum/psi0Norm) :+0)) . psi0Form
            psi0    = psi0Form
            waveT   = takeTil2D 10 $ cn2D sys dr dt psi0
--             title = "harmpot/data_norm_coupl_split_dx" ++ printf "%.4f" dx
--                     ++ "_dt" ++ printf "%.3f" dt
--         _ <- createProcess $ shell $ "mkdir -p output/" ++ title
        -- plotWaveset waveT (-5,5) (-0.1,1.1) $ title ++ "/"
        plotWaveset2D "output/2Dtest/" waveT
--         writeWaveset waveT $ "output/" ++ title ++ ".dat"
--         _ <- plotEnergy sys waveT $ title ++ "/"
--         _ <- plotDensity waveT $ title ++ "/"
        return waveT

m,a,g :: Double
m       = 7.4 -- 2.6 m_e
a       = 0.1
-- a = 0 :: Double
g       = 5*10**(-4)
-- g = 0
u :: Potential2D Double
u _     = 0
int :: Interval2D Double
int = ((-5,-5),(5,5))

sys :: System2D Double
sys = System2D int m u g

sigma,mux,muy :: Double
sigma   = 0.2
mux     = 0
muy     = 0

pnum :: Double
pnum    = 10

psi0Form :: (Double,Double) -> Complex Double
psi0Form (x,y)  = (1/sigma) * exp(-((x-mux)**2+(y-muy)**2)/(2*sigma**2)) :+ 0

dx,dt :: Double
dx      = 0.05
dt      = 0.1
dr :: (Double,Double)
dr = (dx,dx)

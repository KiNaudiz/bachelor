module Test2D
where

import CN2D
import CNTypes
import CNBase
import Data.Complex
import Plot2D
import Plot
import Properties

harmOsz2D :: IO (Waveset2D Double)
harmOsz2D = do
        let
            psi0Norm    = density2Dkarth dr
                        $ renderWave2D int dr psi0Form
            psi0    = (* (sqrt (pnum/psi0Norm) :+0)) . psi0Form
            waveT   = takeTil2D 40 $ cn2D sys dr dt psi0
        -- _ <- plotWaveset2D "output/2Dtest/" [CBRange (0,2)] waveT
        -- _ <- plotWaveset2D "output/2Dtest/" [] waveT
        -- plotOverTime [] dt $ map (density2Dkarth dr) $ wset2DWaves waveT
        plotOverTime [] dt $ map (energy2D sys dr) $ wset2DWaves waveT
        return waveT

m,a,g :: Double
m       = 7.4 -- 2.6 m_e
a       = 0.1
-- a = 0 :: Double
g       = 5*10**(-4)
-- g = 0
u :: Potential2D Double
u (x,y) = a*(x**2+y**2)
int :: Interval2D Double
int = ((-6,-6),(6,6))

sys :: System2D Double
sys = System2D int m u g

sigma,mux,muy :: Double
sigma   = 0.5
mux     = 2
muy     = 2

pnum :: Double
pnum    = 10

psi0Form :: (Double,Double) -> Complex Double
psi0Form (x,y)  = (1/sigma) * exp(-((x-mux)**2+(y-muy)**2)/(2*sigma**2)) :+ 0

dx,dt :: Double
dx      = 0.1
dt      = 0.25
dr :: (Double,Double)
dr = (dx,dx)

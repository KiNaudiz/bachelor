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
            pnnorm  = density2Dkarth dr
                        $ renderWave2D int dr $ normalForm psigma pmu
            psi0    = (* (sqrt (pnum/pnnorm) :+0)) . normalForm psigma pmu
            waveT   = takeTil2D 40 $ cn2D subdiv sys dr dt psi0
        _ <- plotWaveset2D "output/2Dtest/testplot4/"
            [CBRange (0,40),Grid]
            waveT
        -- _ <- plotWaveset2D "output/2Dtest/" [] waveT
        plotOverTime [] dt $ map (density2Dkarth dr) $ wset2DWaves waveT
        plotOverTime [] dt $ map (energy2D sys dr) $ wset2DWaves waveT
        -- print $ map (energy2D sys dr) $ wset2DWaves waveT
        -- print $ map (density2Dkarth dr) $ wset2DWaves waveT
        return waveT

m,a,g :: Double
m       = 7.4 -- 2.6 m_e
a       = 0.1
-- a = 0 :: Double
g       = 5*10**(-4)
-- g = 0
u :: Potential2D Double
u r@(x,y) = a*(x**2+y**2) + g * magnitude ( cond r )
int :: Interval2D Double
int = ((-15,-15),(15,15))
drain :: Double
drain = 5

sys :: System2D Double
sys = System2D int m u g drain

subdiv :: Int
subdiv = 2

psigma :: Double
psigma  = 0.5
pmu :: (Double,Double)
pmu     = (2,2)
csigma :: Double
csigma  = 0.5
cmu :: (Double,Double)
cmu     = (0,0)

pnum,cnum :: Double
pnum    = 1000
cnum    = pnum / 5

cond :: (Double,Double) -> Wavepoint Double
cond    = (* (sqrt (cnum/cnnorm) :+0)) . normalForm csigma cmu
    where cnnorm  =
              density2Dkarth dr $ renderWave2D int dr $ normalForm csigma cmu

normalForm :: Double -> (Double,Double) -> (Double,Double) -> Complex Double
normalForm sigma (mux,muy) (x,y)  =
        (1/sigma) * exp(-((x-mux)**2+(y-muy)**2)/(2*sigma**2)) :+ 0

dx,dt :: Double
dx      = 0.1
dt      = 0.5
dr :: (Double,Double)
dr = (dx,dx)

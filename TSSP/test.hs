import TSSP
import Plot
import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Tuple

main :: IO ()
main = do
        let m       = 7.4 -- 2.6 m_e
            a       = 0.1 :: Double -- µeV µm²
            -- a = 0
            g       = 5*10**(-4) :: Double -- µeV µm³
            u x     = a*x**2 -- harm
            int     = (-40,40)
            system  = System int m u g

            sigma   = 0.5
            mu      = 10
            psi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0

            dx      = 0.05
            dt      = 0.5

            waveT   = tssp system psi0 dx dt
        -- putStr $ unlines $ map show list
        plotWaveset waveT "harmpot/"
        return ()

plotWaveset :: (Show a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => Waveset a -> String -> IO ()
plotWaveset set fname = do
        let list = wsetWaves set
            dt   = wsetDt set
            dx   = wsetDx set
            x0   = wsetX0 set
        plotManyComplex [XLabel "x/um",YLabel "|psi|^2",XRange (-20,20),YRange (-0.1,0.5)] fname list x0 dt dx

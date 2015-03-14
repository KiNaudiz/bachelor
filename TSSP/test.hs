import TSSP
import Plot
import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Tuple

main :: IO ()
main = harmOszSphere
-- main = hydrogen
-- main = harmOsz

harmOszSphere :: IO ()
harmOszSphere = do
        let m       = 7.4 -- 2.6 m_e
            a       = 0.1 :: Double -- µeV µm²
            -- a       = 0 :: Double -- µeV µm²
            -- g       = 5*10**(-4) :: Double -- µeV µm³
            -- g       = 0 :: Double -- µeV µm³ testing
            g       = 5*10**(-2) :: Double -- µeV µm³ testing
            u x     = a*x**2 -- harm
            int     = (0,60)
            system  = System int m u g

            sigma   = 1
            mu      = 10
            -- psi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0
            psi0 x  = 1/sqrt(sqrt pi*sigma) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0

            dx      = 0.05
            dt      = 0.5

            waveT   = tsspSphere system psi0 dx dt
            list    = wsetWaves waveT
            densT   = map ( (*dx) . sum . map ((**2) . magnitude) ) list
        -- putStr $ unlines $ map show list
        -- putStr $ unlines $ map show densT
        plotWaveset waveT "harmpot_sphere/gh_"
        return ()

-- hydrogen :: IO ()
-- hydrogen = do
--         let m       = 1/0.351764 -- 2.6 m_e
--             g       = 5*10**(-4) :: Double -- µeV µm³
--             e0      = 2.8372*10**(-24) :: Double
--             e       = 1.6022*10**(-10) :: Double
--             u x     = e/(4*pi*e0 * x**2)
--             int     = (0,10**(-4))
--             system  = System int m u g
--
--             -- sigma   = 10
--             -- mu      = 100
--             -- psi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0
--             a0      = 5.29*10**(-5)
--             -- psi0 x  = 1/sqrt(sqrt pi*sigma) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0
--             psi0 x  = sqrt(1/(pi*a0**3)) * exp(-x/a0) :+ 0 -- 1 0 0
--             -- psi0 x  = sqrt(2/a0) * exp(-x/a0) :+ 0 -- 2 0 0
--
--             dx      = 10**(-7)
--             dt      = 0.001
--
--             waveT   = tsspSphere system psi0 dx dt
--             list    = wsetWaves waveT
--             densT   = map ( (*dx) . sum . map ((**2) . magnitude) ) list
--         -- putStr $ unlines $ map show list
--         -- putStr $ unlines $ map show densT
--         plotWaveset waveT "hydrogen/"
--         return ()

-- harmOsz :: IO ()
-- harmOsz = do
--         let m       = 7.4 -- 2.6 m_e
--             a       = 0.1 :: Double -- µeV µm²
--             -- a = 0
--             g       = 5*10**(-4) :: Double -- µeV µm³
--             u x     = a*x**2 -- harm
--             int     = (-40,40)
--             system  = System int m u g
--
--             sigma   = 0.5
--             mu      = 10
--             psi0 x  = 1/sqrt(2*pi*sigma**2) * exp(-(x-mu)**2/(2*sigma**2)) :+ 0
--
--             dx      = 0.05
--             dt      = 0.5
--
--             waveT   = tssp system psi0 dx dt
--             list    = wsetWaves waveT
--             densT   = map ( (*dx) . sum . map ((**2) . magnitude) ) list
--         -- putStr $ unlines $ map show list
--         -- plotWaveset waveT "harmpot/"
--         putStr $ unlines $ map show densT
--         return ()

plotWaveset :: (Show a,Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => Waveset a -> String -> IO ()
plotWaveset set fname = do
        let list = wsetWaves set
            dt   = wsetDt set
            dx   = wsetDx set
            x0   = wsetX0 set
        plotManyComplex [XLabel "x/um",YLabel "|psi|^2",XRange (-0,13),YRange (-0.1,5.0)] fname list x0 dt dx
        -- plotManyComplex [XLabel "x/um",YLabel "|psi|^2"] fname list x0 dt dx

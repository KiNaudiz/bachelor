module Plot2D
where

import CNTypes
import ValueMatrix

import Data.Complex
import Text.Printf

import Graphics.Gnuplot.Advanced
import Graphics.Gnuplot.Plot.ThreeDimensional
import Graphics.Gnuplot.Frame as F
import Graphics.Gnuplot.Frame.OptionSet
import Graphics.Gnuplot.Value.Tuple
import Graphics.Gnuplot.Value.Atom
import Graphics.Gnuplot.Terminal.PNG as PNG
import Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

plotWave2D ::   (Graphics.Gnuplot.Value.Tuple.C a
                ,Graphics.Gnuplot.Value.Atom.C a, RealFloat a, PrintfArg a)
    => (a,a) -> (a,a) -> Wave2D a -> F.T (Graph3D.T Int Int a)
plotWave2D (x0,y0) (dx,dy) wave =
        F.cons (viewMap $
                xTicks3d ( genTicks x0 dx 5 nx ) $
                yTicks3d ( genTicks y0 dy 5 ny )
                deflt)
            (surface ([1..nx]::[Int]) ([1..ny]::[Int]) (curry (magnitude . (#) wave)))
    where (nx,ny) = dim wave

genTicks :: (PrintfArg a, Num a) => a -> a -> Int -> Int -> [(String,Int)]
genTicks x0 dx ticknum n = gen' [0..(n-1)]
    where
        part = n `div` ticknum
        gen' []         = []
        gen' (lh:ll)
            | lh `mod` part == 0
                = (printf "%.0f" (x0+fromIntegral lh*dx),lh+1) : gen' ll 
            | otherwise
                = gen' ll

plotWave2DDefault ::   (Graphics.Gnuplot.Value.Tuple.C a
                ,Graphics.Gnuplot.Value.Atom.C a, RealFloat a, PrintfArg a)
    => (a,a) -> (a,a) -> Wave2D a -> IO ()
plotWave2DDefault r0 dr w = do
        _ <- plotDefault $ plotWave2D r0 dr w
        return ()

plotWave2DPNG ::   (Graphics.Gnuplot.Value.Tuple.C a
                ,Graphics.Gnuplot.Value.Atom.C a, RealFloat a, PrintfArg a)
    => (a,a) -> (a,a) -> FilePath -> Wave2D a -> IO ()
plotWave2DPNG r0 dr filename w = do
        _ <- plot (PNG.cons filename) $ plotWave2D r0 dr w
        return ()

plotWaveset2D :: (Graphics.Gnuplot.Value.Atom.C a
        , Graphics.Gnuplot.Value.Tuple.C a,PrintfArg a,RealFloat a)
    => FilePath -> Waveset2D a -> IO ()
plotWaveset2D filename wset = do
        let wl  = wset2DWaves wset
        plot' wl 0
        return ()
    where   r0  = wset2DR0 wset
            dr  = wset2DDr wset
            dt  = wset2DDt wset
            plot' [] _      = return ()
            plot' (lh:ll) t = do
                plotWave2DPNG r0 dr
                    (filename ++ printf "%07.03f" t ++ ".png") lh
                plot' ll (t+dt)

module Plot2D
where

import CNTypes
import ValueMatrix

import Data.Complex
import Text.Printf

import Graphics.Gnuplot.Advanced
import Graphics.Gnuplot.Plot.ThreeDimensional
import Graphics.Gnuplot.Frame as F
import Graphics.Gnuplot.Frame.OptionSet as OptS
import Graphics.Gnuplot.Frame.Option as Opt
import Graphics.Gnuplot.Value.Tuple
import Graphics.Gnuplot.Value.Atom
import Graphics.Gnuplot.Terminal.PNG as PNG
import Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

data PlotOption a =
          XRange (Int,Int)
        | YRange (Int,Int)
        | ZRange (a,a)
        | XYRange ((a,a),(a,a)) (a,a)
        -- | Ratio Double
        | CBRange (a,a)
        | CBLog
        | Grid

plotWave2D ::   (Show a,Graphics.Gnuplot.Value.Tuple.C a
                ,Graphics.Gnuplot.Value.Atom.C a, RealFloat a, PrintfArg a)
    => (a,a) -> (a,a) -> [PlotOption a] -> Wave2D a
    -> F.T (Graph3D.T Int Int a)
plotWave2D (x0,y0) (dx,dy) opt wave =
        F.cons (viewMap $
                xTicks3d ( genTicks x0 dx 5 nx ) $
                yTicks3d ( genTicks y0 dy 5 ny ) $
                applyOpts opt)
            (surface ([1..nx]::[Int]) ([1..ny]::[Int])
                (curry (magnitude . (#) wave)))
                -- (curry (phase . (#) wave)))
    where (nx,ny) = dim wave

applyOpt :: (Show x,Graphics.Gnuplot.Value.Tuple.C x, Graphics.Gnuplot.Value.Atom.C x)
    => PlotOption x -> OptS.T (Graph3D.T Int Int x) -> OptS.T (Graph3D.T Int Int x)
applyOpt (XRange int)       = xRange3d int
applyOpt (YRange int)       = yRange3d int
applyOpt (ZRange int)       = zRange3d int
-- applyOpt (Ratio r)          = sizeSquare
applyOpt (CBLog)  =
        add (custom "logscale cb" "") [""]
applyOpt (CBRange (z0,ze))  =
        add (custom "cbrange" "") ["[" ++ show z0 ++ ":" ++ show ze ++ "]"]
applyOpt (Grid) =
        OptS.grid True

applyOpts :: (Show x,Graphics.Gnuplot.Value.Tuple.C x, Graphics.Gnuplot.Value.Atom.C x)
    => [PlotOption x] -> OptS.T (Graph3D.T Int Int x)
applyOpts   = foldr applyOpt deflt

genTicks :: (PrintfArg a, Num a) => a -> a -> Int -> Int -> [(String,Int)]
genTicks x0 dx ticknum n = gen' [0..(n-1)]
    where
        part = n `div` ticknum
        gen' []         = []
        gen' (lh:ll)
            | lh `mod` part == 0
                = (printf "%.0f" (x0+fromIntegral lh*dx),fromIntegral (lh+1)) : gen' ll
            | otherwise
                = gen' ll

plotWave2DDefault ::   (Show a,Graphics.Gnuplot.Value.Tuple.C a
                ,Graphics.Gnuplot.Value.Atom.C a, RealFloat a, PrintfArg a)
    => (a,a) -> (a,a) -> [PlotOption a] -> Wave2D a -> IO ()
plotWave2DDefault r0 dr opt w = do
        _ <- plotDefault $ plotWave2D r0 dr opt w
        return ()

plotWave2DPNG ::   (Show a,Graphics.Gnuplot.Value.Tuple.C a
                ,Graphics.Gnuplot.Value.Atom.C a, RealFloat a, PrintfArg a)
    => (a,a) -> (a,a) -> FilePath -> [PlotOption a] -> Wave2D a -> IO ()
plotWave2DPNG r0 dr filename opt w = do
        _ <- plot (PNG.cons filename) $ plotWave2D r0 dr opt w
        return ()

plotWaveset2D :: (Show a,Graphics.Gnuplot.Value.Atom.C a
        , Graphics.Gnuplot.Value.Tuple.C a,PrintfArg a,RealFloat a)
    => FilePath -> [PlotOption a] -> Waveset2D a -> IO ()
plotWaveset2D filename opt wset = do
        let wl  = wset2DWaves wset
        plot' wl 0
        return ()
    where   r0  = wset2DR0 wset
            dr  = wset2DDr wset
            dt  = wset2DDt wset
            plot' [] _      = return ()
            plot' (lh:ll) t = do
                plotWave2DPNG r0 dr
                    (filename ++ printf "%07.03f" t ++ ".png") opt lh
                plot' ll (t+dt)

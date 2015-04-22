module Plot2D
where

import CNTypes
import ValueMatrix

import Data.Complex
import Text.Printf

import Graphics.Gnuplot.Advanced
import Graphics.Gnuplot.Plot.ThreeDimensional
import Graphics.Gnuplot.Frame
import Graphics.Gnuplot.Frame.OptionSet
import Graphics.Gnuplot.Value.Tuple
import Graphics.Gnuplot.Value.Atom

plotWave2D ::   (Graphics.Gnuplot.Value.Tuple.C a
                ,Graphics.Gnuplot.Value.Atom.C a, RealFloat a, PrintfArg a)
    => (a,a) -> (a,a) -> Wave2D a -> IO ()
plotWave2D (x0,y0) (dx,dy) wave = do
    let (nx,ny) = dim wave
    _ <- plotDefault $
        cons (viewMap $
                xTicks3d ( genTicks x0 dx 5 nx ) $
                yTicks3d ( genTicks y0 dy 5 ny )
                deflt)
        (surface ([1..nx]::[Int]) ([1..ny]::[Int]) (curry (magnitude . (#) wave)))
    return ()

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

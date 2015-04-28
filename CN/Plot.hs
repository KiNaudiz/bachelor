module Plot
where

import Data.Complex
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Value.Tuple
import Text.Printf

plotComplex :: (Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a)
    => [Attribute] -> [Complex a] -> a -> a -> IO ()
plotComplex a l x0 dx = do
        let real = addLoc x0 $ map realPart l
            imag = addLoc x0 $ map imagPart l
            density = addLoc x0 $ map ((**2) . magnitude) l
        plotLists a [real,imag,density];
        -- plotLists a [density];
    where   addLoc = addPar dx

addPar :: (Num a) => a -> a -> [b] -> [(a,b)]
addPar dx = addPar'
    where   addPar' _ []        = []
            addPar' x (lh:ll)   = (x,lh) : addPar' (x+dx) ll

plotManyComplex :: (Graphics.Gnuplot.Value.Tuple.C a, RealFloat a, Num a,Text.Printf.PrintfArg a)
    => [Attribute] -> String -> [[Complex a]] -> a -> a -> a -> IO ()
plotManyComplex a fname l x0 dt dx = p l 0
    where
        p [] _ = return ()
        p (lh:ll) t = do
            -- plotComplex [PNG ("output/plot"++show t),Title ("t="++show t++"ns"),XLabel="x/us",YLabel="phi"] lh dx
            plotComplex (a ++ [Key Nothing,PNG ("output/"++fname++printf "%07.3f" t++".png"),Title ("t="++printf "%07.3f" t++"ns")]) lh x0 dx
            p ll (t+dt)

plotOverTime :: (Graphics.Gnuplot.Value.Tuple.C a,Num a)
    => [Attribute] -> a -> [a] -> IO ()
plotOverTime attr dt = plotList attr . addPar dt 0

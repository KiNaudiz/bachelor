module Properties
where

import Data.Complex
import CNTypes
import Vector
import ValueMatrix

import Control.Arrow
import Data.Array.Base
import Data.List

import CN2D
import CNBase
import Coupling

densityKarth :: RealFloat a => a -> Wave a -> a
densityKarth = (. (sum . map ((** 2) . magnitude) . fillVec)) . (*)

density2Dkarth :: RealFloat a => (a,a) -> Wave2D a -> a
density2Dkarth (dx,dy) = (*dy) . sum . fillVec . fmap (densityKarth dx) . cols

energy2D :: RealFloat a => System2D a -> (a,a) -> Wave2D a -> a
energy2D sys (dx,dy) wave = kin + pot + coup
    where   v           = effPot2D sys coupl2DKarth
            pot         = safesum $ map (uncurry v . first mkR) $ assocs $ vmat wave
            ((x0,y0),_) = sys2DInterval sys
            mkR (i,j)   = (x0+fromIntegral i*dx,y0+fromIntegral j*dy)

            m           = sys2DMass sys
            diffm       = sndDiffKarthDx dx $! sndDiffKarthDy dy wave
            kin         = (dx*dy*hbar/(2*m) *) $ magnitude $ safesum $ elems $
                vmat $! diffm

            g           = sys2DCoupling sys
            coup        = (g *) $ safesum $ map magnitude $ elems $ vmat wave

safesum :: (Num a) => [a] -> a
safesum = foldl' (+) 0

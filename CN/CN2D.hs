module CN2D
where

import CNTypes
import CNBase
import ValueMatrix
import Tridiag
import Data.Complex
import Control.Applicative

sndDiffKarthDx :: (RealFloat a) => a -> Wave2D a -> Wave2D a
sndDiffKarthDx dx wave =
        fmap (/(dx:+0)**2) $ fromRows $ (diff' -*) <$> rows wave
    where   diff' = diffMtx $ snd $ ValueMatrix.dim wave

sndDiffKarthDy :: (RealFloat a) => a -> Wave2D a -> Wave2D a
sndDiffKarthDy dy = transpose . sndDiffKarthDx dy . transpose

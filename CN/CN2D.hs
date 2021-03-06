{-# LANGUAGE BangPatterns #-}
module CN2D
where

import Data.Array.Base
import CNTypes
import CNBase
import Coupling
import ValueMatrix
import Tridiag
-- import Vector
import Data.Complex
import Control.Applicative
import Control.Arrow

import Control.DeepSeq.Generics

sndDiffKarthDx,sndDiffKarthDy :: (RealFloat a) => a -> Wave2D a -> Wave2D a
sndDiffKarthDy dy wave =
        fmap (/(dy:+0)**2) $ fromRows $! (diff' -*) <$> rows wave
    where   diff' = diffMtx $ snd $ ValueMatrix.dim wave
-- sndDiffKarthDx dx = transpose . sndDiffKarthDx dx . transpose
sndDiffKarthDx dx wave =
        fmap (/(dx:+0)**2) $ fromCols $! (diff' -*) <$> cols wave
    where   diff' = diffMtx $ fst $ ValueMatrix.dim wave

cn2DkinStepX,cn2DkinStepY :: (RealFloat a)
    => System2D a -> a -> a -> Wave2D a -> Wave2D a
cn2DkinStepY sys dy dt wave =
        fromRows $ applKin <$> rows wave
    where
        i               = 0:+1
        hbar_c          = hbar :+ 0
        m_c             = sys2DMass sys :+ 0
        dy_c            = dy :+ 0
        dt_c            = dt :+ 0
        ((x0,_),(xe,_)) = sys2DInterval sys
        int             = (x0,xe)
        n               = waveEntries int dy
        dmtx            = diffMtx n
        mmtx            = idMx n + ((1/12) .** dmtx)
        applKin !w'  = solve lmx b
            where   b       = (mmtx -* w') + (lmx' -* w')
                    lmx'    = (i*dt_c*hbar_c/(4*m_c*dy_c**2)) .** dmtx
                    lmx     = mmtx - lmx'
cn2DkinStepX sys dx dt = transpose . cn2DkinStepY sys dx dt . transpose

cn2D' :: (RealFloat a,NFData a)
    => Int -> System2D a -> (a,a) -> a -> Coupling2D a -> Wave2D a -> Waveset2D a
cn2D' subdiv sys dr@(dx,dy) dt' coupl wave0 =
        Waveset2D (iterate nts wave0) dr dt' r0
    where
        n               = 2^subdiv
        dt              = dt'/fromIntegral n
        (r0,_)          = sys2DInterval sys

        timestep'  p'   = applyHPot p' . applyKin . applyHPot p'
        nts  w          = iterate timestep w !! n

        -- -- predictor CN avg
        -- timestep  w     = timestep' p' w
        --     where   p'  = 0.5 .* (timestep' w w + w)

        -- no predictor
        timestep  w     = deepseq w (timestep' w w)

        applyKin        =   cn2DkinStepY sys dy dt
                          . cn2DkinStepX sys dx dt
        applyHPot       = cn2DhpotStep sys dr dt coupl

effPot2D :: (RealFloat a)
    => System2D a -> ( (a,a) -> Wavepoint a -> a ) -> (a,a) -> Wavepoint a -> a
effPot2D system coupl r phi = sys2DPotential system r
    + sys2DCoupling system * coupl r phi

cn2DhpotStep :: (RealFloat a)
    => System2D a -> (a,a) -> a -> Coupling2D a -> Wave2D a -> Wave2D a -> Wave2D a
cn2DhpotStep sys (dx,dy) dt coupl wave_p wave =
        VM $ array (bounds $ vmat wave) res
    where   vals        = assocs $ vmat wave_p
            pot         = effPot2D sys coupl
            drain       = sys2DDrain sys
            vals'       =
                map (uncurry (cn2DapplyHalfPot drain pot dt) . first mkR) vals
            ((x0,y0),_) = sys2DInterval sys
            mkR (i,j)   = (x0+fromIntegral i*dx,y0+fromIntegral j*dy)
            res         = zipWith (\x (i,y) -> (i,x*y)) vals'
                $ assocs $ vmat wave

cn2DapplyHalfPot :: (RealFloat a)
    => a -> ((a,a) -> Wavepoint a -> a) -> a -> (a,a) -> Wavepoint a
    -> Wavepoint a
cn2DapplyHalfPot drain pot dt r phi =
        exp(-dt_c/2 * (i * v / hbar_c + 1/(drain:+0)))
    where   v   = pot r phi :+ 0
            i   = 0:+1
            hbar_c  = hbar :+ 0
            dt_c    = dt :+ 0

renderWave2D :: (RealFloat a)
    => Interval2D a -> (a,a) -> ((a,a) -> Wavepoint a) -> Wave2D a
renderWave2D (r0,re) (dx,dy) wave0 =
        VM $ array ((1,1),(nx,ny)) vals
    where   xint    = (fst r0,fst re)
            yint    = (snd r0,snd re)
            nx      = waveEntries xint dx
            ny      = waveEntries yint dy
            ns      =
                [(mx',my') | mx' <- [1..nx],my' <- [1..ny]]
            vals    =
                map (\i@(mx',my') ->
                        (i,wave0 (fst r0 + fromIntegral (mx'-1)*dx,snd r0
                                    + fromIntegral (my'-1)*dy))) ns

cn2D :: (RealFloat a,NFData a)
    => Int -> System2D a -> (a,a) -> a -> ((a,a) -> Wavepoint a) -> Waveset2D a
cn2D subdiv system dr dt wave0 =
        cn2D' subdiv system dr dt coupl2DKarth wave0'
    where   int     = sys2DInterval system
            wave0'  = renderWave2D int dr wave0

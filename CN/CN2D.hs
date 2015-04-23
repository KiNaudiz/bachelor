module CN2D
where

import Data.Array.Base
import CNTypes
import CNBase
import ValueMatrix
import Tridiag
-- import Vector
import Data.Complex
import Control.Applicative
import Control.Arrow

sndDiffKarthDx,sndDiffKarthDy :: (RealFloat a) => a -> Wave2D a -> Wave2D a
sndDiffKarthDy dy wave =
        fmap (/(dy:+0)**2) $ fromRows $ (diff' -*) <$> rows wave
    where   diff' = diffMtx $ snd $ ValueMatrix.dim wave
sndDiffKarthDx dx = transpose . sndDiffKarthDx dx . transpose

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
        applKin w'  = solve lmx b
            where   b       = (mmtx -* w') + (lmx' -* w')
                    lmx'    = (i*dt_c*hbar_c/(4*m_c*dy_c**2)) .** dmtx
                    lmx     = mmtx - lmx'
cn2DkinStepX sys dx dt = transpose . cn2DkinStepY sys dx dt . transpose

cn2D' :: (RealFloat a)
    => System2D a -> (a,a) -> a -> Wave2D a -> Waveset2D a
cn2D' sys dr@(dx,dy) dt wave0 =
        Waveset2D (iterate timestep wave0) dr dt r0
    where
        (r0,_)          = sys2DInterval sys

        timestep' p'     = applyHPot p' . applyKin . applyHPot p'

        -- -- predictor CN avg
        -- timestep  w     = timestep' p' w
        --     where   p'  = 0.5 .* (timestep' w w + w)

        -- no predictor
        timestep  w     = timestep' w w

        applyKin        =   cn2DkinStepY sys dy dt
                          . cn2DkinStepX sys dx dt
        applyHPot       = cn2DhpotStep sys dr dt

-- effPot2D :: (RealFloat a)
--     => System2D a -> ( a -> Wavepoint a -> a ) -> a -> Wavepoint a -> a
-- effPot2D system coupl x phi = sysPotential system x
--     + sysCoupling system * coupl x phi

cn2DhpotStep :: (RealFloat a)
    => System2D a -> (a,a) -> a -> Wave2D a -> Wave2D a -> Wave2D a
cn2DhpotStep sys (dx,dy) dt wave_p wave =
        VM $ array (bounds $ vmat wave) res
    where   vals        = assocs $ vmat wave_p
            pot r _     = sys2DPotential sys r -- TODO
            vals'       =
                map (uncurry (cn2DapplyHalfPot pot dt) . first mkR) vals
            ((x0,y0),_) = sys2DInterval sys
            mkR (i,j)   = (x0+fromIntegral i*dx,y0+fromIntegral j*dy)
            res         = zipWith (\x (i,y) -> (i,x*y)) vals'
                $ assocs $ vmat wave

cn2DapplyHalfPot :: (RealFloat a)
    => ((a,a) -> Wavepoint a -> a) -> a -> (a,a) -> Wavepoint a -> Wavepoint a
cn2DapplyHalfPot pot dt r phi =
        exp(-i * dt_c * v / ( 2 * hbar_c ))
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

cn2D :: (RealFloat a)
    => System2D a -> (a,a) -> a -> ((a,a) -> Wavepoint a) -> Waveset2D a
cn2D system dr dt wave0 =
        cn2D' system dr dt wave0'
    where   int     = sys2DInterval system
            wave0'  = renderWave2D int dr wave0

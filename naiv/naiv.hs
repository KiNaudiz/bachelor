-- naive solution of
--   ie d(phi)/dt = -e^2/2 d^2(phi)/dx^2 + U(x) phi
-- asatz:
--   d(phi)/dt = (phi(t+dt) - phi(t))/dt
--   d^2(phi)/dx^2 = (phi(x-dx)-2 phi(x) + phi(t,x+dx))/dx^2

import Data.Complex

timestep :: (RealFloat a, Num a) =>
    [Complex a] -> a -> a -> ( Complex a -> Complex a ) -> a -> [Complex a]
timestep [] _ _ _ _ = []
timestep (lh:ls) dt dx u e = lh : xstep lh ls
    where   --xstep :: (Num a) => Complex a -> [Complex a] -> [Complex a]
            xstep _ []          = []
            xstep _ (s1:[])    = [s1]
            xstep s0 (s1:s2:ss) = calc s0 s1 s2 dt dx u e : xstep s1 (s2:ss)

calc :: (RealFloat a, Num a) => Complex a -> Complex a -> Complex a -> a -> a
    -> ( Complex a -> Complex a) -> a -> Complex a
calc s0 s1 s2 dt dx u e =
        (0:+1) * dt' *
        ( e' * ( s0 - 2*s1 + s2 ) /  (2*dx'**2) -
          u s1 * s1 / e' )
    where
        e'  = e:+0
        dt' = dt:+0
        dx' = dx:+0

initFromList :: (RealFloat a, Num a) =>
    [Complex a] -> a -> a -> ( Complex a -> Complex a ) -> a -> [[Complex a]]
initFromList l dt dx u e =
        iterate (\x -> timestep x dt dx u e) l

main :: IO ()
main = do
    let u x     = x*x
        e       = 1 :: Double
        dt      = 1 :: Double
        dx      = 0.1 :: Double
        phi0    = [0,1,2,4,2,1,0] :: [Complex Double]
        phiT    = take 4 $ initFromList phi0 dt dx u e :: [[Complex Double]]
    print phiT
    return ()

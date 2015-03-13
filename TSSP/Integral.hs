module Integral
where

type Quadratur a = (a -> a) -> a -> a -> a

integral :: (RealFloat a,Integral b)
    => Quadratur a -> (a -> a) -> (a,a) -> b -> a
integral quad f (x0,xe) n =
        sum $ map (quad f h) l
    where   h   = (xe-x0)/fromIntegral n
            l   = [ x0 + h*fromIntegral m | m<-[0..n-1]]

listIntegral :: (RealFloat a)
    => [a] -> a -> a
listIntegral l dx =
        sum $ init $ map (*dx) l

quadSimpson :: (Fractional a) => Quadratur a
quadSimpson f h x =
        h/6 * ( f x + 4 * f x' + f (x+h) )
    where   x' = x + h/2

quadRight :: (Num a) => Quadratur a
quadRight f h x =
        f (x+h) * h

quadLeft :: (Num a) => Quadratur a
quadLeft f h x =
        f x * h

quadTrap :: (Fractional a) => Quadratur a
quadTrap f h x =
        (f x + f (x+h)) * h/2

quadMid :: (Fractional a) => Quadratur a
quadMid f h x =
        f x' * h
    where   x' = x + h/2

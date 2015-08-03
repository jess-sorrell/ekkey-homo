{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Poly where

import Data.List
import Data.Reflection
import System.Random
import Data.Proxy
import Text.Printf
import Zn


{-- Polynomials are parameterized by c, the index of the cyc by which they're
    modded out, and by a list of coefficients, least significant first --}
data Poly c a = Poly {getCoeffs :: [a]}


instance (Integral a, Show a) => Show (Poly c a) where
       show x = foldl (++) "f(x) = " $ map show $ getCoeffs x


{-- making Num a lists instances of num
 -- Code taken from https://wiki.haskell.org/Blow_your_mind --}
instance Num a => Num [a] where               

   (f:fs) + (g:gs) = f+g : fs+gs              
   fs + [] = fs                               
   [] + gs = gs                               

   (f:fs) * (g:gs) = f*g : [f]*gs + fs*(g:gs)
   _ * _ = []                                 

   abs           = undefined   -- I can't think of a sensible definition
   signum        = map signum
   fromInteger n = [fromInteger n]
   negate        = map (\x -> -x)


{-- Poly is an instance of num --}
instance (Integral a, Reifies c Integer) => Num (Poly c a) where
      Poly f + Poly g = Poly (modPolyByCyclo (reflect (Proxy::Proxy c)) (f + g))
      Poly f - Poly g = Poly (modPolyByCyclo (reflect (Proxy::Proxy c)) (f-g))
      Poly f * Poly g = Poly (modPolyByCyclo (reflect (Proxy::Proxy c)) f*g )
      negate (Poly f) = Poly (negate f)
      abs = id
      signum (Poly [zero]) = Poly [0]
      signum _ = Poly [1]
      fromInteger x = Poly (modPolyByCyclo p [fromInteger x])
            where p = reflect (Proxy :: Proxy c)


{-- Integer represents the index of the cyclotomic polynomial that we are
    using as a modulus. Should be a power of two or this is meaningless --}
modPolyByCyclo :: (Integral a) => Integer -> [a] -> [a]
modPolyByCyclo index coeffs = secondFold firstFold
-- First fold collapses the list onto twice the dimension of the result,
-- giving it the same dimension as the Integer argument.
-- Second fold collapses the list onto its proper dimension, subtracting the
-- coefficients given in the second half of the list from those in the first.
-- Since we're modding out by the nth cyclotomic polynomial, x^(n/2) + 1,
-- x^(n/2) = -1, therefore
-- the coefficients from n/2 to n-1 should be subtracted from the 0 to (n/2 -1) -- coefficients, the coefficients from n to n + (n/2 -1) should be added
-- etc, etc
  where firstFold = [sum [ coeffs!!x | x <- [0.. (genericLength coeffs) - 1], (toInteger x) `mod` index == i ] | i <- [0..(index - 1)] ] 
        secondFold = (\x -> (genericTake (index `quot` (toInteger 2)) x) - ( genericDrop (index `quot` (toInteger 2)) x))


reduce :: (Integral a) => a -> Integer -> [a] -> [a]
reduce modulus index coeffs = secondFold firstFold
  where firstFold = [sum [ coeffs!!x | x <- [0.. (genericLength coeffs) - 1], (toInteger x) `mod` index == i ] `mod` modulus | i <- [0..(index - 1)]  ] 
        secondFold = (\ys -> (genericTake (index `quot` (toInteger 2)) ys) - ( genericDrop (index `quot` (toInteger 2)) ys))




convolve :: (Integral a) => a -> Integer -> [a] -> [a] -> [a]
convolve modulus index p1 p2 = reduce modulus index (p1*p2)


likePolyProxy :: Proxy c -> Poly c a -> Poly c a
likePolyProxy _ = id


polyToList :: Integral a => Poly c a -> [a]
polyToList (Poly x) = map fromIntegral x


withCycl :: Integral a => a -> (forall c. Reifies c a => Poly c a) -> [a]
withCycl index poly =
  reify index $ \proxy -> polyToList . likePolyProxy proxy $ poly 

{--
getRandom :: (Integral a, Random a, RandomGen  g) => a -> Integer -> g -> [a]
getRandom = getCenteredRandoms --}


znListToIntList :: Integral a => [Zn s a] -> [a]
znListToIntList [Zn x] = [(fromIntegral x)]
znListToIntList ((Zn x):zns) = (fromIntegral x):(znListToIntList zns)


-- | Convince the compiler that the phantom type in the proxy
-- | is the same as the one in the Zn
likeProxyZns :: Proxy s -> [Zn s a] -> [Zn s a]
likeProxyZns _ = id


withZnList :: Integral a => a -> (forall s. Reifies s a => [Zn s a]) -> [a]
withZnList modulus zs =
  reify modulus $ \proxy -> znListToIntList.likeProxyZns proxy $ zs
                            



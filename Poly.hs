{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

--import Math.Polynomial
import Data.List
import Data.Reflection
import Data.Proxy
import Text.Printf
import Zn

{-- Polynomials are parameterized by c, the cyclotomic index by which they're
    modded out, and by an [a] list of coefficients, least significant first --}
newtype Poly c a = Poly {getCoeffs :: [a]}


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
      Poly f + Poly g = Poly (modPolyByCyclo (f + g) (reflect (Proxy::Proxy c)))
      Poly f - Poly g = Poly (modPolyByCyclo (f - g) (reflect (Proxy::Proxy c)))
      Poly f * Poly g = Poly (modPolyByCyclo (f * g) (reflect (Proxy::Proxy c)))
      negate (Poly f) = Poly (negate f)
      abs = id
      signum (Poly [zero]) = Poly [0]
      signum _ = Poly [1]
      fromInteger x = Poly (modPolyByCyclo [fromInteger x] p)
            where p = reflect (Proxy :: Proxy c)


{-- Integer represents the index of the cyclotomic polynomial that we are
    using as a modulus. Should be a power of two or this is meaningless --}
modPolyByCyclo :: (Integral a) => [a] -> Integer -> [a]
modPolyByCyclo coeffs index = secondFold firstFold
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




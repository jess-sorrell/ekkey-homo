{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

--import Math.Polynomial
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
      fromInteger x = Poly ( modPolyByCyclo [fromInteger x] p)
            where p = reflect (Proxy :: Proxy c)


{-- Integer represents the index of the cyclotomic polynomial that we are
    using as a modulus. Should be a power of two or this is meaningless --}
modPolyByCyclo :: (Integral a) => [a] -> Integer -> [a]
modPolyByCyclo coeffs index
  | (length coeffs) < index/2 = coeffs
  | otherwise = undefined

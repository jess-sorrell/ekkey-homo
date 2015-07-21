{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

--import Math.Polynomial
import Data.Reflection
import Data.Proxy
import Text.Printf
import Zn

newtype Cycl c a = Cycl {getCoeffs :: [a]}

instance (Integral a, Show a) => Show (Cycl c a) where
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


{-- Cycl is an instance of num --}
instance (Num a, Reifies c Integer) => Num (Cycl c a) where
      Cycl f + Cycl g = Cycl (modPoly (f + g) (reflect (Proxy::Proxy c)))
      Cycl f - Cycl g = Cycl (modPoly (f - g) (reflect (Proxy::Proxy c)))
      Cycl f * Cycl g = Cycl (modPoly (f * g) (reflect (Proxy::Proxy c)))
      negate (Cycl f) = Cycl (negate f)
      abs = id
      signum (Cycl [zero]) = Cycl [0]
      signum _ = Cycl [1]
      fromInteger x = Cycl ( modPoly [fromInteger x] p)
            where p = reflect (Proxy :: Proxy c)


modPoly :: (Num a) => [a] -> Integer -> [a]
modPoly = undefined

{-# LANGUAGE FlexibleInstances #-}

module Poly where

import Data.List
import System.Random
import Zn


-- Polynomials are parameterized by n, the cyclotomic index of the
-- polynomial by which they're modded out. This should always be a power
-- of 2.
data Poly c a = Poly c [a] deriving (Eq, Show)


polyFromList :: (Integral a, Integral c) => c-> [Zn a] -> Poly c (Zn a)
polyFromList c1 ((Zn m1 x):xs) =
  case isPowerOfTwo c1 of
    True ->
      if (genericLength xs < dim)
      then Poly c1 (xs ++ genericReplicate (dim - genericLength xs)(Zn m1 0))
      else reduce $ Poly c1 xs
      where 
        dim = c1 `div` 2
    False ->
      error "Invalid index for Poly"


zeroZnPoly :: (Num a, Integral c) => c -> a -> Poly c (Zn a)
zeroZnPoly c1 m1 =
  case isPowerOfTwo c1 of
    True ->
      Poly c1 $ genericReplicate dim $ Zn m1 0
      where dim = c1 `div` 2
    False ->
      error "Invalid index for Poly"

      
      
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


-- Poly c (Zn a) is an instance of num
instance (Integral a, Integral c) => Num (Poly c (Zn a)) where
  Poly c1 f + Poly c2 g
          | c1 == c2 = reduce $ Poly c1 (f + g)
          |otherwise = undefined
  Poly c1 f - Poly c2 g 
          | c1 == c2 = reduce $ Poly c1 (f - g)
          |otherwise = undefined
  Poly c1 f * Poly c2 g 
          | c1 == c2 = reduce $ Poly c1 (f * g)
          | otherwise = undefined
  negate (Poly c1 f) = reduce $ Poly c1 (negate f)
  abs = id
  signum (Poly c1 [zero]) = Poly c1 [0]
  signum (Poly c1 _) = Poly c1 [1]
  fromInteger x = error "No dimension provided"


isPowerOfTwo :: (Integral a) => a -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo n
  | n `mod` 2 == 0 = isPowerOfTwo $ n `div` 2
  | otherwise = False


reduce :: (Integral a, Integral c) => Poly c (Zn a) -> Poly c (Zn a)
reduce (Poly c1 f@((Zn m1 x):xs)) =
  case isPowerOfTwo c1 of
    True -> Poly c1 $ secondFold firstFold
      where 
          dim = c1 `div` 2
          firstFold =
            [foldl (+) (Zn m1 0) [ (genericIndex f x) | x <- [0..(genericLength f) - 1], x `mod` c1 == i] | i <- [0..(c1 - 1)]  ]
          secondFold =
            (\ys -> (genericTake dim ys) - ( genericDrop dim ys))          
    False -> error "Invalid index for Poly"
    


-- Poly is an instance of random, but an index and modulus  will need to
-- be provided
instance (Random a, Integral a, Integral c) =>
         Random (c -> a -> Poly c (Zn a)) where
  random g =
    let (g', g'') = split g
    in ((\c1 m -> case isPowerOfTwo c1 of
            True -> Poly c1 $ map (\f -> f m)
                    (genericTake (c1 `div` 2) (randoms g'))), g'')

  randomR = error "Range is not meaningfully defined for Poly"


randomPoly :: (RandomGen g, Random a, Integral a, Integral c) =>
              g -> c -> a -> (Poly c (Zn a), g)
randomPoly gen c1 m =
  let ( zns, gen') = 
        foldl (\(zs, g) _ ->
                let (z, g') = randomZn g m
                in ( (z:zs), g')) ([], gen) [1..(c1 `div` 2)]
  in (Poly c1 zns, gen')
  

-- Modulus switching for a polynomial
polySwitch :: (Integral a, Integral c) => a -> Poly c (Zn a) -> Poly c (Zn a)
polySwitch targ (Poly c1 p) =
  Poly c1 $ map (switch targ) p


polyLength :: (Integral c) => Poly c a -> c
polyLength (Poly c1 xs) = genericLength xs

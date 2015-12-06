{-# LANGUAGE FlexibleInstances #-}

module Poly where

import Data.List
import System.Random
import Zn


-- Polynomials are parameterized by n, the cyclotomic index of the
-- polynomial by which they're modded out. This should always be a power
-- of 2.
data Poly c a = Poly c [a] deriving (Eq, Show)


polyFromList :: (Num a, Integral c) => c-> [a] -> Poly c a
polyFromList c1 xs =
  case isPowerOfTwo c1 of
    True ->
      if (genericLength xs < dim)
      then Poly c1 (xs ++ genericReplicate (dim - genericLength xs) 0)
      else reduce $ Poly c1 xs
      where 
        dim = c1 `div` 2
    False ->
      error "Invalid index for Poly"


zeroPoly :: (Num a, Integral c) => c -> Poly c a
zeroPoly c1 =
  case isPowerOfTwo c1 of
    True ->
      Poly c1 (genericReplicate dim $fromInteger 0)
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


-- Poly is an instance of num
instance (Num a, Integral c) => Num (Poly c a) where
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


reduce :: (Num a, Integral c) => Poly c a -> Poly c a
reduce (Poly c1 f) =
  case isPowerOfTwo c1 of
    True -> Poly c1 $ secondFold firstFold
      where 
          dim = c1 `div` 2
          firstFold =
            [sum [ (genericIndex f x) | x <- [0..(genericLength f) - 1], x `mod` c1 == i] | i <- [0..(c1 - 1)]  ]
          secondFold =
            (\ys -> (genericTake dim ys) - ( genericDrop dim ys))          
    False -> error "Invalid index for Poly"
    


-- Poly is an instance of random, but an index will need to be provided
instance (Random a, Num a, Integral c) => Random (c -> Poly c a) where
  random g =
    let (g', g'') = split g
    in ((\x ->
          Poly x $ genericTake x $ randoms g'), g'')
  randomR = error "Range is not meaningfully defined for Poly"



                



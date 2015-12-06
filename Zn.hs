{-# LANGUAGE FlexibleInstances #-}

module Zn where

import System.Random

data Zn a = Zn a a deriving (Show, Eq, Ord)

-- Zn is an instance of Num
instance (Integral a) => Num (Zn a) where
  Zn m1 x + Zn m2 y
            | m1 == m2 = Zn m1 $ (x + y) `mod` m1
            | otherwise = error "Incompatible Moduli"
  Zn m1 x * Zn m2 y
            | m1 == m2 = Zn m1 $ (x * y) `mod` m1
            | otherwise = error "Incompatible Moduli"
  abs (Zn m1 x) = Zn m1 (x `mod` m1)
  signum _ = error "Sign is meaningless"
  fromInteger x = error "No modulus provided" 
  negate (Zn m1 x) = Zn m1 $ (negate x) `mod` m1


-- Zn is an instance of Random, but a modulus will need to be provided
instance (Integral a, Random a) => Random (a -> Zn a) where
  random g =
    let (g', g'') = split g
    in ((\x ->
          Zn x $ fst $ randomR (0, x-1) g'), g'')
  randomR = error "Range of (a -> Zn a) undefined"


-- For binary expansion of Zn
quot2 :: (Integral a) => Zn a -> Zn a
quot2 (Zn m1 x) = Zn m1 $ quot x $ fromInteger 2

rem2 :: (Integral a) => Zn a -> Zn a
rem2 (Zn m1 x) = Zn m1 $ rem x $ fromInteger 2

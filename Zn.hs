{-# LANGUAGE FlexibleInstances #-}

module Zn where

import System.Random

data Zn a = Zn a a deriving (Eq, Ord)


-- Zn is an instance of Show
instance (Show a) => Show (Zn a) where
  show (Zn m1 x) = show x

  
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
    in ((\m ->
          Zn m $ fst $ randomR (0, m-1) g'), g'')
  randomR = error "Range of (a -> Zn a) undefined"


randomZn :: (Integral a, Random a, RandomGen g) => g -> a -> (Zn a, g)
randomZn g m1 =
  let (f, g') = random g
  in (f m1, g')


-- For binary expansion of Zn
quot2 :: (Integral a) => Zn a -> Zn a
quot2 (Zn m1 x) = Zn m1 $ quot x 2

rem2 :: (Integral a) => Zn a -> Zn a
rem2 (Zn m1 x) = Zn m1 $ rem x 2


-- Switch modulus
switch :: (Integral a) => a -> Zn a -> Zn a
switch m1 (Zn m2 x) = Zn m1 $ round ( toRational m1 * toRational x/toRational m2) `mod` m1

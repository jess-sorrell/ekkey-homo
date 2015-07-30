{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Zn
import Poly


get_get_gadget :: (Integral a) => a -> [Zn s a] 
get_get_gadget modulus =
  let l = ceiling $ logBase 2 $ fromIntegral modulus
  in [Zn (2^i) | i <- [0..l]]

go_go_gadget_expand :: (Integral a, Reifies s a) => Zn s a -> [Zn s a]
go_go_gadget_expand (Zn 0) = [Zn 0]
go_go_gadget_expand (Zn 1) = [Zn 1]
go_go_gadget_expand (Zn x) = (Zn ( x `mod` 2)):(go_go_gadget_expand $ Zn (x `quot` 2))


go_go_gadget_vector :: (Integral a, Reifies s a) => [Zn s a] -> [Zn s a]
go_go_gadget_vector [] = []
go_go_gadget_vector (x:xs) = (go_go_gadget_expand x)++(go_go_gadget_vector xs)


go_go_gadget_compose :: (Integral a, Reifies s a) => Integer -> [Zn s a] -> [Zn s a] -> [Zn s a]
go_go_gadget_compose index as expanded
  | length expanded == length as = convolve index as expanded
  | length expanded `mod` length as == 0 =
    let dim = length as
    in (convolve index as (take dim expanded))++
       (go_go_gadget_compose index as (drop dim expanded))
  | otherwise = error "What do you want me to do with this?"

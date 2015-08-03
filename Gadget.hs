{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Gadget where

import Data.Reflection
import Data.Proxy
import Text.Printf
import System.Random
import Zn
import Poly


get_get_gadget :: (Integral a) => a -> [a] 
get_get_gadget modulus =
  let l = ceiling $ logBase 2 $ fromIntegral modulus
  in [ 2^i | i <- [0..l]]

go_go_gadget_expand :: (Integral a) => a -> [a]
go_go_gadget_expand 0 = [0]
go_go_gadget_expand 1 = [1]
go_go_gadget_expand x = ( x `mod` 2):(go_go_gadget_expand $ x `quot` 2)


go_go_gadget_vector :: (Integral a) => [a] -> [a]
go_go_gadget_vector [] = []
go_go_gadget_vector (x:xs) = (go_go_gadget_expand x)++(go_go_gadget_vector xs)


go_go_gadget_compose :: (Integral a) => Integer -> [a] -> [a] -> [a]
go_go_gadget_compose index as expanded
  | length expanded == length as = convolve index as expanded
  | length expanded `mod` length as == 0 =
    let dim = length as
    in (convolve index as (take dim expanded))++
       (go_go_gadget_compose index as (drop dim expanded))
  | otherwise = error "What do you want me to do with this?"


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
  let l = floor $ logBase 2 $ fromIntegral modulus
  in [ 2^i | i <- [0..l]]


go_go_gadget_elem :: (Integral a) => a -> [a]
go_go_gadget_elem 0 = [0]
go_go_gadget_elem 1 = [1]
go_go_gadget_elem x = (go_go_gadget_elem $ x `quot` 2)++[(x `mod` 2)]

{--
go_go_gadget_inverse' :: (Integral a) => [a] -> [[a]]
go_go_gadget_inverse' as
  | (filter ( <= 1) as) == as = [as]
  | otherwise = (map (\x -> x `mod` 2) as):(go_go_gadget_inverse' $ map (\x -> x `quot` 2) as) --}

go_go_gadget_inverse' :: (Integral a) => [a] -> [[a]]
go_go_gadget_inverse' as = undefined



go_go_gadget_inverse :: (Integral a) => a -> [a] -> [[a]]
go_go_gadget_inverse modulus as =
  let expand = go_go_gadget_inverse' $ map (\x -> x `mod` modulus ) as
  in
   let pad = (floor $ logBase 2 $ fromIntegral modulus) - (length expand)
   in
    expand++([ [0 | i <- [0..((length as) - 1)]] | j <- [0..pad]])




{--
go_go_gadget_vector :: (Integral a, Integral b) => b -> [a] -> [a]
go_go_gadget_vector modulus [] = []
go_go_gadget_vector modulus (x:xs) = (go_go_gadget_pad modulus x)++(go_go_gadget_vector modulus xs) --}

go_go_gadget_invector :: (Integral a) => a -> [[a]] -> [[[a]]]
go_go_gadget_invector modulus vec = map (go_go_gadget_inverse modulus) vec


go_go_gadget_pad :: (Integral a, Integral b) => b -> a -> [a]
go_go_gadget_pad modulus x =
  let xs = go_go_gadget_elem x
  in
   let pad = (floor $ logBase 2 $ fromIntegral modulus) - (length xs)
   in
    ([0 | i <- [0..pad]]++xs)


go_go_gadget_compose :: (Integral a, Integral b) => a -> b -> [[a]] -> [[[a]]] -> [[a]]
go_go_gadget_compose modulus dim row mat = map (polydot modulus dim row) mat


polydot :: (Integral a, Integral b) => a -> b -> [[a]] -> [[a]] -> [a]
polydot modulus dim v1 v2 = reduce modulus dim $ foldl (+) [0 | i <- [0..((length v1)-1)]] $ zipWith (convolve modulus dim) v1 v2



{--
go_go_gadget_compose' :: (Integral a, Integral b) => a -> b -> [a] -> [[a]] -> [a]
go_go_gadget_compose' modulus index as expanded --}

{--
go_go_gadget_compose :: (Integral a, Integral b) => a -> b -> [a] -> [a] -> [a]
go_go_gadget_compose modulus index as expanded
  | length expanded == length as = convolve modulus index as expanded
  | length expanded `mod` length as == 0 =
    let dim = length as
    in (convolve modulus index as (take dim expanded))++
       (go_go_gadget_compose modulus index as (drop dim expanded))
  | otherwise = error "What do you want me to do with this?"


go_go_gadget_product :: (Integral a, Integral b) => a -> b -> [a] -> [a] -> [a]
go_go_gadget_product modulus index as bs =
  go_go_gadget_compose modulus index as (go_go_gadget_vector modulus bs)
--}
